--  with Hera.Calendar;
with Hera.Money;
with Hera.Quantities;
with Hera.Updates;

with Hera.Accounts;
with Hera.Pops.People;

with Hera.Colonies.Transactions;

with Hera.Facilities.Updates;

package body Hera.Installations.Updates is

   type Queue_Update_Type is
     new Hera.Objects.Root_Update_Type with
      record
         New_Queue : Hera.Facilities.Production_Queue;
      end record;

   overriding procedure Execute
     (Update : Queue_Update_Type;
      Object : in out Hera.Objects.Root_Hera_Object'Class);

   overriding function Description
     (Update : Queue_Update_Type)
      return String
   is ("update queue");

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Update : Queue_Update_Type;
      Object : in out Hera.Objects.Root_Hera_Object'Class)
   is
      Installation : Root_Installation_Type'Class renames
        Root_Installation_Type'Class (Object);
   begin
      Installation.Queue := Update.New_Queue;
   end Execute;

   ------------------------
   -- Execute_Production --
   ------------------------

   procedure Execute_Production
     (Installation : Installation_Type)
   is
      Queue : Hera.Facilities.Production_Queue := Installation.Queue;
   begin
      Queue.Clear_Changed;
      Hera.Facilities.Updates.Execute_Production
        (Colony   => Installation.Colony,
         Owner    => Installation.Owner,
         Stock    => Installation.all,
         Facility => Installation.Facility,
         Employer => Installation,
         Queue    => Queue);

      if Queue.Changed then
         Installation.Add_Update
           (Queue_Update_Type'
              (Hera.Objects.Root_Update_Type with
               New_Queue => Queue));
      end if;

   end Execute_Production;

   ------------------
   -- Hire_Workers --
   ------------------

   procedure Hire_Workers (Installation : Installation_Type) is
      use Hera.Quantities;
      Facility : constant Hera.Facilities.Facility_Type :=
        Installation.Facility;
      Colony   : constant Hera.Colonies.Colony_Type :=
        Installation.Colony;
   begin
      for Worker of Facility.Workers loop
         declare
            Required     : constant Quantity_Type :=
                             Facility.Quantity (Worker);
            Available    : constant Quantity_Type :=
              Colony.Employees (Installation, Worker);
         begin
            if Available < Required then
               declare
                  Hired : constant Quantity_Type :=
                    Colony.Hire_Workers
                      (Employer  => Installation,
                       Class     => Worker,
                       Quantity  => Required - Available,
                       Salary    => Worker.Salary);
               begin
                  Installation.Log
                    (Worker.Tag & ": required "
                     & Show (Required)
                     & "; have "
                     & Show (Available)
                     & "; available "
                     & Show (Colony.Unemployed (Worker).Size)
                     & "; hired "
                     & Show (Hired));
               end;
            end if;
         end;
      end loop;
   end Hire_Workers;

   -----------------
   -- Pay_Workers --
   -----------------

   procedure Pay_Workers
     (Installation : Installation_Type)
   is
      procedure Process_Workers
        (Workers : Hera.Pops.People.Population_Type);

      ---------------------
      -- Process_Workers --
      ---------------------

      procedure Process_Workers
        (Workers : Hera.Pops.People.Population_Type)
      is
         use Hera.Money;
         Total_Salary : constant Money_Type :=
           Adjust (Total (Workers.Class.Salary, Workers.Size),
                   1.0 / Hera.Updates.Update_Cycle_Days);
         Transaction  : constant Hera.Accounts.Transaction_Type :=
           Hera.Colonies.Transactions.Salary_Transaction
             (Employer => Installation,
              Employee => Workers,
              Total    => Total_Salary);
      begin
         Installation.Account.Spend
           (Transaction => Transaction,
            Amount      => Total_Salary);
         Workers.Account.Earn
           (Transaction => Transaction,
            Amount      => Total_Salary);
      end Process_Workers;

   begin
      Installation.Colony.Iterate_Workers
        (Installation, Process_Workers'Access);
   end Pay_Workers;

end Hera.Installations.Updates;
