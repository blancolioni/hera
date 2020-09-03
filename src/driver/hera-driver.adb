with Ada.Calendar;
with Ada.Directories;
with Ada.Text_IO;

with WL.Command_Line;
with WL.Localisation;
with WL.Processes;

with Tropos.Writer;

with Hera.Logging;
with Hera.Options;
with Hera.Paths;
with Hera.Profiling;
with Hera.Real_Images;

with Hera.Calendar;
with Hera.Money;
with Hera.Quantities;

with Hera.Commands.Loader;

with Hera.Scenarios;

with Hera.Updates.Control;

with Hera.Accounts;
with Hera.Commodities;
with Hera.Corporations;
with Hera.Markets;
with Hera.Star_Systems;

with Hera.UI.Web_UI;

procedure Hera.Driver is

   Updates_Running : Boolean := False;

begin

   if not Ada.Directories.Exists ("local.options") then
      Ada.Directories.Copy_File
        (Source_Name => Hera.Paths.Config_File ("default-options.txt"),
         Target_Name => "local.options");
   end if;

   WL.Command_Line.Load_Defaults ("local.options");

   WL.Localisation.Read_Localisation_Directory
     (WL.Localisation.To_Language (Hera.Options.Language),
      Hera.Paths.Config_File ("localisation"));

   Hera.Logging.Start_Logging;

   declare
      Scenario : constant Hera.Scenarios.Hera_Scenario :=
        Hera.Scenarios.Open_Scenario ("default");
      pragma Unreferenced (Scenario);
   begin
      null;
   end;

   Ada.Text_IO.Put_Line
     ("Start date: " & Hera.Calendar.Image (Hera.Calendar.Clock));

   Ada.Text_IO.Put_Line ("starting server ...");

   Hera.Updates.Control.Start_Updates;
   Updates_Running := True;

   if Hera.Options.Batch_Mode then

      Ada.Text_IO.Put_Line ("H E R A");

      declare
         Process     : WL.Processes.Process_Type;
         Update_Days : constant Natural :=
           Hera.Options.Update_Count;
         Start : constant Ada.Calendar.Time := Ada.Calendar.Clock;
      begin
         Process.Start_Bar ("Updating", Update_Days * 24, True);

         for Day_Index in 1 .. Update_Days loop
            for Hour_Index in 1 .. 24 loop
               for Minute_Index in 1 .. 60 loop
                  Hera.Calendar.Advance (60.0);
                  Hera.Updates.Control.Execute_Pending_Updates;
               end loop;
               Process.Tick;
            end loop;

            declare
               procedure Log_Status
                 (Corporation : Hera.Corporations.Corporation_Type);

               ----------------
               -- Log_Status --
               ----------------

               procedure Log_Status
                 (Corporation : Hera.Corporations.Corporation_Type)
               is

                  use Hera.Calendar;

                  procedure Log_Transaction_Class
                    (Name : String;
                     Amount : Hera.Money.Money_Type);

                  ---------------------------
                  -- Log_Transaction_Class --
                  ---------------------------

                  procedure Log_Transaction_Class
                    (Name   : String;
                     Amount : Hera.Money.Money_Type)
                  is
                  begin
                     Corporation.Log
                     (Name & " " & Hera.Money.Show (Amount));
                  end Log_Transaction_Class;

                  Books : constant Hera.Accounts.Transaction_Summary :=
                    Corporation.Account.Get_Transaction_Summary
                      (From => Clock - Days (1),
                       To   => Clock - 1.0);
               begin
                  Hera.Accounts.Iterate
                    (Books, Log_Transaction_Class'Access);

                  Corporation.Log
                    ("cash: " & Hera.Money.Show (Corporation.Cash));
               end Log_Status;

            begin
               Hera.Corporations.Iterate (Log_Status'Access);
            end;

         end loop;

         Process.Finish;

         declare
            use Ada.Calendar;
            Elapsed : constant Duration := Clock - Start;
         begin
            Ada.Text_IO.Put_Line
              ("advanced" & Update_Days'Image & " days in "
               & Hera.Real_Images.Approximate_Image
                 (Real (Elapsed)) & "s");
         end;

      end;

   else

      Hera.Commands.Loader.Load_Commands;

      declare
         UI : constant Hera.UI.UI_Interface'Class :=
           Hera.UI.Web_UI.Get_Web_UI;
      begin
         UI.Start;
         Ada.Text_IO.Put ("Press return to exit");
         Ada.Text_IO.Flush;
         Ada.Text_IO.Skip_Line;
         UI.Stop ("exiting");
      end;
   end if;

   Updates_Running := False;
   Hera.Updates.Control.Stop_Updates;

   Ada.Text_IO.Put_Line
     ("Stop date: " & Hera.Calendar.Image (Hera.Calendar.Clock));

   declare
      use Hera.Money;

      Total      : Money_Type := Zero;
      Total_Debt : Money_Type := Zero;
      Total_Cash : Money_Type := Zero;

      procedure Log_Account (Account : Hera.Accounts.Account_Type);

      -----------------
      -- Log_Account --
      -----------------

      procedure Log_Account (Account : Hera.Accounts.Account_Type) is
         Cash : constant Money_Type := Account.Current_Cash;
      begin
         Total := Total + Cash;
         if Cash < Zero then
            Total_Debt := Total_Debt - Cash;
         else
            Total_Cash := Total_Cash + Cash;
         end if;
         if False then
            Account.Log ("balance: " & Show (Cash));
         end if;
      end Log_Account;

   begin
      Hera.Accounts.Iterate_Accounts (Log_Account'Access);
      Ada.Text_IO.Put_Line
        ("total value: " & Show (Total) & " cash: " & Show (Total_Cash)
             & " debt: " & Show (Total_Debt));
   end;

   declare
      procedure Log_Trade_Center
        (Name                    : String;
         Production, Consumption : Hera.Markets.Trade_Node_Type);

      ----------------------
      -- Log_Trade_Center --
      ----------------------

      procedure Log_Trade_Center
        (Name                    : String;
         Production, Consumption : Hera.Markets.Trade_Node_Type)
      is

         procedure Log_State
           (Commodity         : Hera.Commodities.Commodity_Type;
            Production_Entry  : Hera.Commodities.Stock_Entry;
            Consumption_Entry : Hera.Commodities.Stock_Entry);

         ---------------
         -- Log_State --
         ---------------

         procedure Log_State
           (Commodity         : Hera.Commodities.Commodity_Type;
            Production_Entry  : Hera.Commodities.Stock_Entry;
            Consumption_Entry : Hera.Commodities.Stock_Entry)
         is
            use Hera.Money, Hera.Quantities;
            Production_Price : constant Price_Type :=
              Price (Production_Entry.Value, Production_Entry.Quantity);
            Consumption_Price : constant Price_Type :=
              Price (Consumption_Entry.Value, Consumption_Entry.Quantity);

         begin
            Commodity.Log
              ("production/consumption: "
               & Hera.Quantities.Show (Production_Entry.Quantity)
               & "/"
               & Hera.Quantities.Show (Consumption_Entry.Quantity)
               & " "
               & Show (Production_Price) & "/" & Show (Consumption_Price)
               & " "
               & Show (Production_Entry.Value)
               & "/"
               & Show (Consumption_Entry.Value));
         end Log_State;

      begin
         Hera.Logging.Log
           (Name, "trade center status");
         Hera.Commodities.Scan_Stock_Join
           (Left    => Production.all,
            Right   => Consumption.all,
            Process => Log_State'Access);
      end Log_Trade_Center;

   begin
      Hera.Markets.Iterate_Trade_Centers (Log_Trade_Center'Access);
   end;

   Hera.Profiling.Report_Profiles;

   Ada.Text_IO.Put_Line
     ("Saving state");

   declare
      State : Tropos.Configuration :=
        Tropos.New_Config ("hera");
   begin
      Hera.Star_Systems.Save (State);
      Hera.Corporations.Save (State);
      Tropos.Writer.Write_Config (State, "save.game");
   end;

   Ada.Text_IO.Put_Line ("exit");

   Hera.Logging.Stop_Logging;

exception

   when others =>
      if Updates_Running then
         Hera.Updates.Control.Stop_Updates;
      end if;

      Hera.Logging.Stop_Logging;
      raise;

end Hera.Driver;
