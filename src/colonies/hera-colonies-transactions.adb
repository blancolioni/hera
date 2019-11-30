package body Hera.Colonies.Transactions is

   type Employment_Transaction_Type is
     new Hera.Accounts.Root_Transaction_Type with
      record
         Employed : Boolean;
         Employer : Employer_Access;
         Quantity : Hera.Quantities.Quantity_Type;
         Class    : Hera.Pops.Classes.Pop_Class_Type;
      end record;

   overriding function Class_Name
     (Transaction : Employment_Transaction_Type)
      return String
   is ("employment");

   overriding function Description
     (Transaction : Employment_Transaction_Type)
      return String
   is (Hera.Quantities.Show (Transaction.Quantity)
       & " " & Transaction.Class.Tag
       & (if Transaction.Employed
          then " employed by "
          else " leave employment by ")
       & Transaction.Employer.Name);

   type Salary_Transaction_Type is
     new Hera.Accounts.Root_Transaction_Type with null record;

   overriding function Class_Name
     (Transaction : Salary_Transaction_Type)
      return String
   is ("salary");

   overriding function Description
     (Transaction : Salary_Transaction_Type)
      return String
   is ("paid salary " & Hera.Money.Show (Transaction.Amount));

   ----------------------------
   -- Employment_Transaction --
   ----------------------------

   function Employment_Transaction
     (From         : Hera.Pops.People.Population_Type;
      To           : Hera.Pops.People.Population_Type;
      New_Employer : not null access constant Employer_Interface'Class;
      Quantity     : Hera.Quantities.Quantity_Type)
      return Hera.Accounts.Transaction_Type
   is
      Proportion : constant Unit_Real :=
        Hera.Quantities.To_Real (Quantity)
        / Hera.Quantities.To_Real (From.Size);
      Transaction : Employment_Transaction_Type :=
        Employment_Transaction_Type'
          (Hera.Accounts.Root_Transaction_Type with
           Employed => True,
           Employer => Employer_Access (New_Employer),
           Quantity => Quantity,
           Class    => From.Class);
   begin
      Transaction.New_Transaction
        (From.Account,
         To.Account,
         Hera.Money.Adjust (From.Cash, Proportion));
      return new Employment_Transaction_Type'(Transaction);
   end Employment_Transaction;

   ------------------------
   -- Salary_Transaction --
   ------------------------

   function Salary_Transaction
     (Employer : not null access constant Employer_Interface'Class;
      Employee : not null access constant
        Hera.Pops.People.Root_Population_Object'Class;
      Total    : Hera.Money.Money_Type) return Hera.Accounts.Transaction_Type
   is
      Salary : Salary_Transaction_Type;
   begin
      Salary.New_Transaction
        (From   => Employer.Account,
         To     => Employee.Account,
         Amount => Total);
      return new Salary_Transaction_Type'(Salary);
   end Salary_Transaction;

   ------------------------------
   -- Unemployment_Transaction --
   ------------------------------

   function Unemployment_Transaction
     (From         : Hera.Pops.People.Population_Type;
      To           : Hera.Pops.People.Population_Type;
      Old_Employer : not null access constant Employer_Interface'Class;
      Quantity     : Hera.Quantities.Quantity_Type)
      return Hera.Accounts.Transaction_Type
   is
      Proportion  : constant Unit_Real :=
        Hera.Quantities.To_Real (Quantity)
        / Hera.Quantities.To_Real (From.Size);
      Transaction : Employment_Transaction_Type :=
        Employment_Transaction_Type'
          (Hera.Accounts.Root_Transaction_Type with
           Employed => False,
           Employer => Employer_Access (Old_Employer),
           Quantity => Quantity,
           Class    => From.Class);
   begin
      Transaction.New_Transaction
        (From.Account, To.Account,
         Hera.Money.Adjust (From.Cash, Proportion));
      return new Employment_Transaction_Type'(Transaction);
   end Unemployment_Transaction;

end Hera.Colonies.Transactions;
