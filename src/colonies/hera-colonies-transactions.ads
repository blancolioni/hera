package Hera.Colonies.Transactions is

   function Employment_Transaction
     (From         : Hera.Pops.People.Population_Type;
      To           : Hera.Pops.People.Population_Type;
      New_Employer : not null access constant Employer_Interface'Class;
      Quantity     : Hera.Quantities.Quantity_Type)
      return Hera.Accounts.Transaction_Type;

   function Unemployment_Transaction
     (From         : Hera.Pops.People.Population_Type;
      To           : Hera.Pops.People.Population_Type;
      Old_Employer : not null access constant Employer_Interface'Class;
      Quantity     : Hera.Quantities.Quantity_Type)
      return Hera.Accounts.Transaction_Type;

   function Salary_Transaction
     (Employer : not null access constant Employer_Interface'Class;
      Employee : not null access constant
        Hera.Pops.People.Root_Population_Object'Class;
      Total    : Hera.Money.Money_Type)
      return Hera.Accounts.Transaction_Type;

end Hera.Colonies.Transactions;
