with Ada.Calendar;
with Ada.Text_IO;

with WL.Work;

with Hera.Calendar;
with Hera.Money;
with Hera.Options;
with Hera.Real_Images;

with Hera.Colonies.Updates;
with Hera.Corporations.Manager;

with Hera.Handles.Account.Selections;
with Hera.Handles.Account_History.Selections;
with Hera.Handles.Colony;
with Hera.Handles.Corporation.Selections;
with Hera.Handles.Salary_Transaction.Selections;
with Hera.Handles.Commodity_Transaction.Selections;

with Hera.Handles.Colony.Selections;

with Hera.Reports;

package body Hera.Updates is

   Use_Work_Tasks      : constant Boolean := False;
   Report_Corporations : constant Boolean := False;

   type Colony_Update_Job is
     new WL.Work.Root_Job_Type with
      record
         Handle : Hera.Handles.Colony.Colony_Handle;
      end record;

   overriding procedure Execute
     (Job : in out Colony_Update_Job);

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Job : in out Colony_Update_Job)
   is
   begin
      Hera.Colonies.Updates.Update (Job.Handle);
   end Execute;

   ------------
   -- Update --
   ------------

   procedure Update is
      Start_Time : constant Hera.Calendar.Time :=
        Hera.Calendar.Clock;
   begin

      Ada.Text_IO.Put_Line
        ("Initializing history ...");
      for Account of Hera.Handles.Account.Selections.Select_All loop
         Hera.Handles.Account_History.Create
           (Account    => Account,
            Time_Stamp => Hera.Calendar.Clock,
            Balance    => Account.Cash);
      end loop;

      Ada.Text_IO.Put_Line
        ("Generating NPC orders ...");

      for Corporation of Hera.Handles.Corporation.Selections.Select_All loop
         Hera.Corporations.Manager.Manage (Corporation);
      end loop;

      if Use_Work_Tasks then

         Ada.Text_IO.Put_Line
           ("Creating update jobs ...");

         WL.Work.Set_Task_Count (Hera.Options.Work_Task_Count);

         declare
            Work_Handle : WL.Work.Work_Handle :=
              WL.Work.Create_Handle;
            Count       : Natural := 0;
         begin
            for Colony of Hera.Handles.Colony.Selections.Select_All loop
               Count := Count + 1;
               WL.Work.Add_Job
                 (Handle => Work_Handle,
                  Job    =>
                     new Colony_Update_Job'
                    (WL.Work.Root_Job_Type with
                     Handle => Colony));
            end loop;

            Ada.Text_IO.Put_Line
              ("Waiting for jobs to finish ...");

            declare
               Start : constant Ada.Calendar.Time :=
                 Ada.Calendar.Clock;
            begin
               WL.Work.Wait (Work_Handle);

               declare
                  use type Ada.Calendar.Time;
                  D : constant Duration :=
                    Ada.Calendar.Clock - Start;
               begin
                  Ada.Text_IO.Put_Line
                    ("completed" & Count'Image
                     & " jobs in "
                     & Hera.Real_Images.Approximate_Image (Real (D))
                     & "s");
               end;
            end;

         end;

         WL.Work.Stop_Work_Tasks;

      else

         Ada.Text_IO.Put_Line
           ("Running update ...");

         declare
            Start : constant Ada.Calendar.Time :=
              Ada.Calendar.Clock;
            Count : Natural := 0;
         begin
            for Colony of Hera.Handles.Colony.Selections.Select_All loop
               Count := Count + 1;
               Hera.Colonies.Updates.Update (Colony);
            end loop;

            declare
               use type Ada.Calendar.Time;
               D : constant Duration :=
                 Ada.Calendar.Clock - Start;
            begin
               Ada.Text_IO.Put_Line
                 ("completed" & Count'Image
                  & " colony updates "
                  & Hera.Real_Images.Approximate_Image (Real (D))
                  & "s");
            end;

         end;

      end if;

      Hera.Calendar.Advance
        (Hera.Calendar.Days (30));

      if Report_Corporations then
         declare
            use Hera.Money;
            use Hera.Reports;
            Finish_Time : constant Hera.Calendar.Time :=
              Hera.Calendar.Clock;
         begin
            Start_Report ("CORPORATIONS");
            Heading ("NAME", 16);
            Heading ("START", 9);
            Heading ("SALARIES", 9);
            Heading ("BOUGHT", 9);
            Heading ("SOLD", 9);
            Heading ("FINISH", 9);

            for Corporation of
              Hera.Handles.Corporation.Selections.Select_All
            loop

               Data (Corporation.Name);

               declare
                  use type Hera.Calendar.Time;
                  use Hera.Handles.Account_History.Selections;
                  Start_Balance : Money_Type := Hera.Money.Zero;
                  End_Balance   : Money_Type := Hera.Money.Zero;
                  Salaries      : Money_Type := Hera.Money.Zero;
                  Bought        : Money_Type := Hera.Money.Zero;
                  Sold          : Money_Type := Hera.Money.Zero;
                  First         : Boolean := True;
               begin
                  for History of Select_Where
                    (Account (Corporation.Account)
                     and Time_Stamp >= Start_Time
                     and Time_Stamp <= Finish_Time)
                  loop
                     if First then
                        Start_Balance := History.Balance;
                        First := False;
                     end if;
                     End_Balance := History.Balance;
                  end loop;

                  declare
                     use Hera.Handles.Salary_Transaction.Selections;
                  begin
                     for Salary of
                       Select_Where
                         (Buyer (Corporation)
                          and Time_Stamp >= Start_Time
                          and Time_Stamp <= Finish_Time)
                     loop
                        Salaries := Salaries + Salary.Amount;
                     end loop;
                  end;

                  declare
                     use Hera.Handles.Commodity_Transaction.Selections;
                  begin
                     for Transaction of
                       Select_Where (Seller (Corporation)
                                     and Time_Stamp >= Start_Time
                                     and Time_Stamp <= Finish_Time)
                     loop
                        Sold := Sold + Transaction.Amount;
                     end loop;
                     for Transaction of Select_Where
                       (Buyer (Corporation)
                        and Time_Stamp >= Start_Time
                        and Time_Stamp <= Finish_Time)
                     loop
                        Bought := Bought + Transaction.Amount;
                     end loop;
                  end;

                  Data (Start_Balance);
                  Data (Salaries);
                  Data (Bought);
                  Data (Sold);
                  Data (End_Balance);
               end;
            end loop;
         end;
      end if;

   end Update;

end Hera.Updates;
