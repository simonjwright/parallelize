--  Copyright (C) 2024 Simon Wright <simon@pushface.org>
--  Licence: Apache 2.0

with Ada.Containers.Ordered_Maps;
with Ada.Containers.Vectors;
with Ada.Exceptions;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with Ada.Unchecked_Conversion;
with GNAT.Command_Line;
with GNAT.OS_Lib;
with GNAT.Strings;
with System.Multiprocessors;

with Commands;

procedure Parallelize is

   --  This program reads a set of commands (jobs) from standard
   --  input, one per line, terminated by end-of-file or a blank
   --  line. It then executes them in parallel subprocesses, where the
   --  number defaults to the number of CPUs available.
   --
   --  Any standard error output from a job is merged with its
   --  standard output. The overall output consists of the output from
   --  each job, in the order in which the jobs were presented.

   --  Unbounded String utilities.
   function "+" (L : String) return Ada.Strings.Unbounded.Unbounded_String
     renames Ada.Strings.Unbounded.To_Unbounded_String;
   function "+" (L : Ada.Strings.Unbounded.Unbounded_String) return String
     renames Ada.Strings.Unbounded.To_String;

   ---  If Verbose is True, this procedure outputs Message via standard error.
   procedure Log (Message : String);

   --  Information on a job.
   type Job is record
      Command          : Ada.Strings.Unbounded.Unbounded_String;
      Executable       : Boolean := False;
      Started          : Boolean := False;
      Finished         : Boolean := False;
      Status           : Boolean := False;
      Complete         : Boolean := False;
      Output_File_Name : GNAT.OS_Lib.String_Access;
   end record;

   --  The index of a Job in the Job Vector, renamed for clarity.
   subtype Job_Index is Positive;

   package Job_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Job_Index,
      Element_Type => Job);
   use Job_Vectors;

   --  Each Job will be run in a Process. We need a map from
   --  Process_Id to Job, so that when the Process completes we can
   --  find the Job it was running. This comparison function is needed
   --  to support an Ordered Map (a Hashed Map would require a hash
   --  function, so _something_ is needed).
   function "<" (L, R : GNAT.OS_Lib.Process_Id) return Boolean;

   package Process_Maps is new Ada.Containers.Ordered_Maps
     (Key_Type     => GNAT.OS_Lib.Process_Id,
      Element_Type => Job_Index);
   use Process_Maps;

   --  This procedure starts as many Jobs as currently possible.
   procedure Start_Processes;

   Command_Line_Config : GNAT.Command_Line.Command_Line_Configuration;

   Verbose       : aliased Boolean := False;
   Max_Processes : aliased Integer
     := Positive (System.Multiprocessors.Number_Of_CPUs);
   --  This has to be an Integer, because of its use in option processing.

   Jobs      : Job_Vectors.Vector;
   Processes : Process_Maps.Map;

   procedure Log (Message : String) is
   begin
      if Verbose then
         Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error, Message);
      end if;
   end Log;

   function "<" (L, R : GNAT.OS_Lib.Process_Id) return Boolean
   is
      --  Process_Id (on Darwin, anyway) is new Integer.
      function Convert
        is new Ada.Unchecked_Conversion (GNAT.OS_Lib.Process_Id, Integer);
   begin
      return Convert (L) < Convert (R);
   end "<";

   procedure Start_Processes is
   begin

      Next_Job :
      for J in Jobs.Iterate loop

         exit Next_Job when Natural (Processes.Length) = Max_Processes;

         if not Jobs (J).Started then

            Set_Up_Job :
            declare
               The_Job : Job renames Jobs (J);
               Whole_Command : constant String := +The_Job.Command;
               Command : constant String
                 := Commands.Command_Part (Whole_Command);
               Executable : GNAT.OS_Lib.String_Access
                 := GNAT.OS_Lib.Locate_Exec_On_Path (Command);
               Pid : GNAT.OS_Lib.Process_Id;
               use type GNAT.OS_Lib.Process_Id;
               Unused_FD : GNAT.OS_Lib.File_Descriptor;
               use type GNAT.Strings.String_Access; -- for Executable
            begin
               The_Job.Started := True;
               if Executable = null then
                  Log ("'" & Command & "' not executable");
               else
                  The_Job.Executable := True;

                  --  This is the temporary file in which we keep the
                  --  Job's output.
                  GNAT.OS_Lib.Create_Temp_Output_File
                    (Unused_FD, The_Job.Output_File_Name);

                  Pid := GNAT.OS_Lib.Non_Blocking_Spawn
                    (Program_Name => Executable.all,
                     Args         => GNAT.OS_Lib.Argument_String_To_List
                       (Commands.Argument_Part (Whole_Command)).all,
                     Output_File  => The_Job.Output_File_Name.all,
                     Err_To_Out   => True);

                  if Pid = GNAT.OS_Lib.Invalid_Pid then
                     --  Not sure why this happened; did we run out of
                     --  processes?
                     Log ("couldn't spawn '" & Whole_Command & "'");
                     GNAT.Strings.Free (The_Job.Output_File_Name);
                  else
                     Log ("starting '" & Whole_Command & "'");
                     Processes.Insert (Key      => Pid,
                                       New_Item => Job_Vectors.To_Index (J));
                  end if;

                  GNAT.Strings.Free (Executable);
               end if;  -- Job was executable
            end Set_Up_Job;

         end if;

      end loop Next_Job;

   end Start_Processes;

begin

   GNAT.Command_Line.Set_Usage
     (Command_Line_Config,
      Usage       => "[switches]",
      Help        => "Run the jobs defined in standard input (one per line)");
   GNAT.Command_Line.Define_Switch
     (Command_Line_Config,
      Verbose'Access,
      "-v",
      Long_Switch => "--verbose",
      Help        => "Report progress");
   GNAT.Command_Line.Define_Switch
     (Command_Line_Config,
      Max_Processes'Access,
      "-j!",
      Long_Switch => "--jobs=",
      Initial     => Positive (System.Multiprocessors.Number_Of_CPUs),
      Help        => "Number of jobs to run in parallel (D=number of CPUs)");

   GNAT.Command_Line.Getopt (Command_Line_Config);
   pragma Assert (Max_Processes in Positive,
                  "the number of processes must be >0");

   Read_Commands :
   loop
      begin

         Read_Command :
         declare
            Input : constant String
              := Ada.Strings.Fixed.Trim
                (Ada.Text_IO.Get_Line, Ada.Strings.Both);
         begin
            exit Read_Commands when Input'Length = 0;
            Jobs.Append (New_Item => Job'(Command => +Input,
                                          others  => <>));
         end Read_Command;

      exception
         when Ada.Text_IO.End_Error =>
            exit Read_Commands;
      end;
   end loop Read_Commands;

   Run :
   declare
      --  Keep track of the Jobs that have been output so far.
      Last_Job_Output : Natural := Job_Index'First - 1;
   begin

      Monitor_Process_Completion :
      loop

         Find_Finished_Process :
         declare
            Finished_Pid : GNAT.OS_Lib.Process_Id;
            use type GNAT.OS_Lib.Process_Id;
            Job_Status : Boolean;
         begin

            --  We get here either because we've just started
            --  (i.e. first time round the Monitor_Process_Completion
            --  loop), in which case we'll start up to Max_Processes
            --  Processes (or the number of Jobs, if less), or because
            --  a Process has just finished, in which case we'll start
            --  one Process (unless we've started all the Jobs).
            Start_Processes;

            GNAT.OS_Lib.Wait_Process (Pid     => Finished_Pid,
                                      Success => Job_Status);

            --  If there are no subprocesses, we're done.
            exit Monitor_Process_Completion
            when Finished_Pid = GNAT.OS_Lib.Invalid_Pid;

            Handle_Completed_Job :
            declare
               Job_ID : constant Job_Index := Processes (Finished_Pid);
               The_Job : Job renames Jobs (Job_ID);
            begin

               Log ("finished '" & (+The_Job.Command) & "'");
               The_Job.Status := Job_Status;
               The_Job.Finished := True;
               Processes.Delete (Finished_Pid);
               --  freeing a slot for another Job, if there are any left

            end Handle_Completed_Job;

            Output_Completed_Jobs_In_Order :
            for J in Last_Job_Output + 1 .. Jobs.Last_Index loop

               --  A Job isn't executable if the Command part (the
               --  first word) isn't executable. In that case, there
               --  was never a Process, not any output.
               if Jobs (J).Executable then

                  --  We're presenting the output, in Job order, of
                  --  each of the Jobs that have Finished. If a job
                  --  hasn't Finished, there will still be output to
                  --  come, so we stop here to wait for it.
                  exit Output_Completed_Jobs_In_Order
                  when not Jobs (J).Finished;

                  Last_Job_Output := J;

                  Output_Job :
                  declare
                     The_Job : Job renames Jobs (J);
                     File : Ada.Text_IO.File_Type;
                  begin

                     --  Copy the Job's output to Parallelize's
                     --  standard output.
                     Ada.Text_IO.Open (File,
                                       Name => The_Job.Output_File_Name.all,
                                       Mode => Ada.Text_IO.In_File);
                     while not Ada.Text_IO.End_Of_File (File) loop
                        Ada.Text_IO.Put_Line (Ada.Text_IO.Get_Line (File));
                     end loop;

                     --  When done, delete the temporary file and its
                     --  name.
                     Ada.Text_IO.Delete (File);
                     GNAT.Strings.Free (The_Job.Output_File_Name);

                     --  This isn't strictly necessary, because next
                     --  time round we continue from the next Job (the
                     --  one after Last_Job_Ouput).
                     The_Job.Complete := True;
                     Log ("completed '" & (+The_Job.Command) & "'");

                  end Output_Job;

               end if;

            end loop Output_Completed_Jobs_In_Order;

         end Find_Finished_Process;

      end loop Monitor_Process_Completion;

   end Run;

exception
   --  This happens when parallelize is called with -h/--help.
   when GNAT.Command_Line.Exit_From_Command_Line => null;
end Parallelize;
