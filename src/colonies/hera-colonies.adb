with Ada.Containers.Vectors;

with Hera.Colonies.Transactions;

package body Hera.Colonies is

   Colony_Version : constant Hera.Objects.Object_Version := "0.0.1";

   package Colony_Vectors is
     new Ada.Containers.Vectors (Positive, Colony_Type);

   Vector : Colony_Vectors.Vector;

   type Add_Population_Update is
     new Hera.Objects.Root_Update_Type with
      record
         Class    : Hera.Pops.Classes.Pop_Class_Type;
         Quantity : Hera.Quantities.Quantity_Type;
         Cash     : Hera.Money.Money_Type;
      end record;

   overriding procedure Execute
     (Update : Add_Population_Update;
      Object : in out Hera.Objects.Root_Hera_Object'Class);

   overriding function Description
     (Update : Add_Population_Update)
      return String
   is ("added " & Hera.Quantities.Show (Update.Quantity)
       & " " & Update.Class.Tag);

   type New_Population_Update is
     new Hera.Objects.Root_Update_Type with
      record
         Pop      : Hera.Pops.People.Population_Type;
         Employer : Employer_Access;
      end record;

   overriding procedure Execute
     (Update : New_Population_Update;
      Object : in out Hera.Objects.Root_Hera_Object'Class);

   overriding function Description
     (Update : New_Population_Update)
      return String
   is ("new "
       & (if Update.Employer = null then "unemployed " else "")
       & "pop " & Update.Pop.Class.Tag
       & (if Update.Employer = null then ""
          else " employed by " & Update.Employer.Name));

   type Add_Warehouse_Update is
     new Hera.Objects.Root_Update_Type with
      record
         Warehouse : Hera.Warehouses.Warehouse_Type;
      end record;

   overriding procedure Execute
     (Update : Add_Warehouse_Update;
      Object : in out Hera.Objects.Root_Hera_Object'Class);

   overriding function Description
     (Update : Add_Warehouse_Update)
      return String
   is ("new warehouse for " & Update.Warehouse.Owner.Name);

   ----------------------------
   -- Add_Initial_Population --
   ----------------------------

   procedure Add_Initial_Population
     (Colony   : Root_Colony_Type'Class;
      Class    : Hera.Pops.Classes.Pop_Class_Type;
      Quantity : Hera.Quantities.Quantity_Type)
   is
   begin
      Colony.Add_Population
        (Class    => Class,
         Quantity => Quantity,
         Cash     => Hera.Money.Zero);
   end Add_Initial_Population;

   -----------------------
   -- Add_Initial_Stock --
   -----------------------

   procedure Add_Initial_Stock
     (Colony    : Root_Colony_Type'Class;
      Owner     : Hera.Corporations.Corporation_Type;
      Commodity : Hera.Commodities.Commodity_Type;
      Quantity  : Hera.Quantities.Quantity_Type)
   is
   begin
      Colony.Warehouse (Owner).Add
        (Commodity, Quantity, Commodity.Base_Price);
   end Add_Initial_Stock;

   --------------------
   -- Add_Population --
   --------------------

   procedure Add_Population
     (Colony   : Root_Colony_Type'Class;
      Class    : Hera.Pops.Classes.Pop_Class_Type;
      Quantity : Hera.Quantities.Quantity_Type;
      Cash     : Hera.Money.Money_Type)
   is
   begin
      Hera.Objects.Add_Update
        (Target => Colony,
         Update =>
           Add_Population_Update'
             (Hera.Objects.Root_Update_Type with
              Class    => Class,
              Quantity => Quantity,
              Cash     => Cash));
   end Add_Population;

   ---------------
   -- Employees --
   ---------------

   function Employees
     (Colony   : Root_Colony_Type'Class;
      Employer : not null access constant Employer_Interface'Class;
      Class    : Hera.Pops.Classes.Pop_Class_Type)
      return Hera.Quantities.Quantity_Type
   is
      use type Hera.Pops.Classes.Pop_Class_Type;
      E : constant Employer_Access := Employer_Access (Employer);
   begin
      for Item of Colony.Pops loop
         if Item.Pop_Class = Class
           and then Item.Employer = E
         then
            return Item.Pop.Size;
         end if;
      end loop;
      return Hera.Quantities.Zero;
   end Employees;

   ---------------
   -- Employees --
   ---------------

   function Employees
     (Colony   : Root_Colony_Type'Class;
      Employer : not null access constant Employer_Interface'Class;
      Class    : Hera.Pops.Classes.Pop_Class_Type)
      return Hera.Pops.People.Population_Type
   is
      use type Hera.Pops.Classes.Pop_Class_Type;
      E : constant Employer_Access := Employer_Access (Employer);
   begin
      for Item of Colony.Pops loop
         if Item.Pop_Class = Class
           and then Item.Employer = E
         then
            return Item.Pop;
         end if;
      end loop;

      declare
         Pop : constant Hera.Pops.People.Population_Type :=
           Hera.Pops.People.New_Population
             (Class, Hera.Quantities.Zero, Hera.Money.Zero,
              Vector.Element (Colony.Index), Colony.Market);
         Class_Tag     : constant String := Class.Tag;
         Employer_Name : constant String := Employer.Name;
      begin
         Colony.Log ("autocreated new " & Class_Tag
                     & " for employer " & Employer_Name);
         Colony.Add_Update
           (New_Population_Update'
              (Hera.Objects.Root_Update_Type with
               Pop      => Pop,
               Employer => Employer_Access (Employer)));
         return Pop;
      end;
   end Employees;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Update : Add_Population_Update;
      Object : in out Hera.Objects.Root_Hera_Object'Class)
   is
      use type Hera.Pops.Classes.Pop_Class_Type;
      Colony : Root_Colony_Type'Class renames
                 Root_Colony_Type'Class (Object);
      Found  : Boolean := False;
   begin

      for Item of Colony.Pops loop
         if Item.Pop_Class = Update.Class
           and then Item.Employer = null
         then
            Item.Pop.Add_Population (Update.Quantity);
            Found := True;
            exit;
         end if;
      end loop;

      if not Found then
         Colony.Pops.Append
           (Pop_Entry'
              (Pop_Class => Update.Class,
               Pop       =>
                 Hera.Pops.People.New_Population
                   (Update.Class, Update.Quantity, Update.Cash,
                    Vector.Element (Colony.Index), Colony.Market),
               Employer  => null));
      end if;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Update : New_Population_Update;
      Object : in out Hera.Objects.Root_Hera_Object'Class)
   is
      Colony : Root_Colony_Type'Class renames
        Root_Colony_Type'Class (Object);
   begin
      Colony.Pops.Append
        (Pop_Entry'
           (Pop_Class => Update.Pop.Class,
            Pop       => Update.Pop,
            Employer  => Update.Employer));
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Update : Add_Warehouse_Update;
      Object : in out Hera.Objects.Root_Hera_Object'Class)
   is
   begin
      Root_Colony_Type (Object).Warehouses.Append (Update.Warehouse);
   end Execute;

   -------------------
   -- Has_Warehouse --
   -------------------

   function Has_Warehouse
     (Colony      : Root_Colony_Type'Class;
      Corporation : Hera.Corporations.Corporation_Type)
      return Boolean
   is
      use type Hera.Corporations.Corporation_Type;
   begin
      for W of Colony.Warehouses loop
         if W.Owner = Corporation then
            return True;
         end if;
      end loop;
      return False;
   end Has_Warehouse;

   ------------------
   -- Hire_Workers --
   ------------------

   function Hire_Workers
     (Colony   : Root_Colony_Type'Class;
      Employer : not null access constant Employer_Interface'Class;
      Class    : Hera.Pops.Classes.Pop_Class_Type;
      Quantity : Hera.Quantities.Quantity_Type;
      Salary   : Hera.Money.Price_Type)
      return Hera.Quantities.Quantity_Type
   is
      use Hera.Quantities;
      Unemployed_Pop : constant Hera.Pops.People.Population_Type :=
        Colony.Unemployed (Class);
      Employed_Pop   : constant Hera.Pops.People.Population_Type :=
        Colony.Employees (Employer, Class);
      Employed_Quantity : constant Quantity_Type :=
        Min (Unemployed_Pop.Size, Quantity);
   begin
      if Employed_Quantity > Zero then
         Unemployed_Pop.Move_Population
           (Employed_Pop,
            Transactions.Employment_Transaction
              (Unemployed_Pop, Employed_Pop,
               Employer, Employed_Quantity),
            Employed_Quantity);
         declare
            Total_Salary   : constant Hera.Money.Money_Type :=
              Hera.Money.Total (Salary, Quantity);
            Initial_Salary : constant Hera.Accounts.Transaction_Type :=
              Hera.Colonies.Transactions.Salary_Transaction
                (Employer, Employed_Pop, Total_Salary);
         begin
            Employed_Pop.Earn
              (Transaction => Initial_Salary,
               Amount      => Total_Salary);
            Employer.Account.Spend
              (Transaction => Initial_Salary,
               Amount      => Total_Salary);
         end;
      end if;
      return Employed_Quantity;
   end Hire_Workers;

   -------------
   -- Iterate --
   -------------

   procedure Iterate
     (Process : not null access
        procedure (Colony : Colony_Type))
   is
   begin
      for Colony of Vector loop
         Process (Colony);
      end loop;
   end Iterate;

   ---------------------
   -- Iterate_Workers --
   ---------------------

   procedure Iterate_Workers
     (Colony   : Root_Colony_Type'Class;
      Employer : not null access constant Employer_Interface'Class;
      Process  : not null access
        procedure (Workers : Hera.Pops.People.Population_Type))
   is
      E : constant Employer_Access := Employer_Access (Employer);
   begin
      for Item of Colony.Pops loop
         if Item.Employer = E then
            Process (Item.Pop);
         end if;
      end loop;
   end Iterate_Workers;

   ----------------
   -- New_Colony --
   ----------------

   function New_Colony
     (Colony : Root_Colony_Type'Class;
      Name   : String)
     return Colony_Type
   is
   begin
      return Result : constant Colony_Type :=
        Colony_Type
          (Colony.New_Named_Object
             (Colony_Version, Name))
      do
         Result.Log ("founded on " & Colony.Planet.Name);
         Vector.Append (Result);
      end return;
   end New_Colony;

   ----------------
   -- Next_Index --
   ----------------

   function Next_Index return Positive is
   begin
      return Vector.Last_Index + 1;
   end Next_Index;

   ----------------
   -- Unemployed --
   ----------------

   function Unemployed
     (Colony   : Root_Colony_Type'Class;
      Class    : Hera.Pops.Classes.Pop_Class_Type)
      return Hera.Pops.People.Population_Type
   is
      use type Hera.Pops.Classes.Pop_Class_Type;
   begin
      for Item of Colony.Pops loop
         if Item.Pop_Class = Class
           and then Item.Employer = null
         then
            return Item.Pop;
         end if;
      end loop;

      declare
         Pop : constant Hera.Pops.People.Population_Type :=
           Hera.Pops.People.New_Population
             (Class, Hera.Quantities.Zero, Hera.Money.Zero,
              Vector.Element (Colony.Index), Colony.Market);
      begin
         Colony.Add_Update
           (New_Population_Update'
              (Hera.Objects.Root_Update_Type with
               Pop      => Pop,
               Employer => null));
         return Pop;
      end;

   end Unemployed;

   ---------------
   -- Warehouse --
   ---------------

   function Warehouse
     (Colony      : Root_Colony_Type'Class;
      Corporation : Hera.Corporations.Corporation_Type)
      return Hera.Warehouses.Warehouse_Type
   is
      use type Hera.Corporations.Corporation_Type;
   begin
      for Warehouse of Colony.Warehouses loop
         if Warehouse.Owner = Corporation then
            return Warehouse;
         end if;
      end loop;
      declare
         Warehouse : constant Hera.Warehouses.Warehouse_Type :=
                       Hera.Warehouses.New_Warehouse
                         (Corporation);
      begin
         Colony.Add_Update
           (Update => Add_Warehouse_Update'
              (Hera.Objects.Root_Update_Type with
               Warehouse => Warehouse));
         return Warehouse;
      end;
   end Warehouse;

end Hera.Colonies;
