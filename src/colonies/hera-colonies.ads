private with Ada.Containers.Doubly_Linked_Lists;

with Hera.Accounts;
with Hera.Commodities;
with Hera.Corporations;
with Hera.Markets;
with Hera.Planets;
with Hera.Pops.Classes;
with Hera.Pops.People;
with Hera.Sectors;
with Hera.Warehouses;

with Hera.Calendar;
with Hera.Money;
with Hera.Quantities;

with Hera.Objects;

package Hera.Colonies is

   type Employer_Interface is interface
     and Hera.Accounts.Has_Account_Interface;

   function Name (Employer : Employer_Interface) return String
                  is abstract;

   type Root_Colony_Type is
     new Hera.Objects.Root_Named_Object
   with private;

   function Government
     (Colony      : Root_Colony_Type'Class)
      return Hera.Corporations.Corporation_Type;

   function Market
     (Colony      : Root_Colony_Type'Class)
      return Hera.Markets.Market_Type;

   function Planet
     (Colony      : Root_Colony_Type'Class)
      return Hera.Planets.Planet_Type;

   function Sector
     (Colony      : Root_Colony_Type'Class)
      return Hera.Sectors.Sector_Type;

   function Has_Warehouse
     (Colony      : Root_Colony_Type'Class;
      Corporation : Hera.Corporations.Corporation_Type)
      return Boolean;

   function Warehouse
     (Colony      : Root_Colony_Type'Class;
      Corporation : Hera.Corporations.Corporation_Type)
      return Hera.Warehouses.Warehouse_Type;

   function Employees
     (Colony   : Root_Colony_Type'Class;
      Employer : not null access constant Employer_Interface'Class;
      Class    : Hera.Pops.Classes.Pop_Class_Type)
      return Hera.Quantities.Quantity_Type;

   function Employees
     (Colony   : Root_Colony_Type'Class;
      Employer : not null access constant Employer_Interface'Class;
      Class    : Hera.Pops.Classes.Pop_Class_Type)
      return Hera.Pops.People.Population_Type;

   function Unemployed
     (Colony   : Root_Colony_Type'Class;
      Class    : Hera.Pops.Classes.Pop_Class_Type)
      return Hera.Pops.People.Population_Type;

   function Hire_Workers
     (Colony   : Root_Colony_Type'Class;
      Employer : not null access constant Employer_Interface'Class;
      Class    : Hera.Pops.Classes.Pop_Class_Type;
      Quantity : Hera.Quantities.Quantity_Type;
      Salary   : Hera.Money.Price_Type)
      return Hera.Quantities.Quantity_Type;

   procedure Iterate_Workers
     (Colony : Root_Colony_Type'Class;
      Employer : not null access constant Employer_Interface'Class;
      Process  : not null access
        procedure (Workers : Hera.Pops.People.Population_Type));

   type Colony_Type is access constant Root_Colony_Type'Class;

   procedure Add_Population
     (Colony   : Root_Colony_Type'Class;
      Class    : Hera.Pops.Classes.Pop_Class_Type;
      Quantity : Hera.Quantities.Quantity_Type;
      Cash     : Hera.Money.Money_Type);

   procedure Iterate
     (Process : not null access
        procedure (Colony : Colony_Type));

   procedure Iterate_Planet_Colonies
     (Planet : not null access constant
        Hera.Planets.Root_Planet_Type'Class;
      Process : not null access
        procedure (Colony : Colony_Type));

private

   type Employer_Access is access constant Employer_Interface'Class;

   type Pop_Entry is
      record
         Pop_Class : Hera.Pops.Classes.Pop_Class_Type;
         Pop       : Hera.Pops.People.Population_Type;
         Employer  : Employer_Access;
      end record;

   package Pop_Lists is
     new Ada.Containers.Doubly_Linked_Lists
       (Pop_Entry);

   package Warehouse_Lists is
     new Ada.Containers.Doubly_Linked_Lists
       (Hera.Warehouses.Warehouse_Type,
        Hera.Warehouses."=");

   type Root_Colony_Type is
     new Hera.Objects.Root_Named_Object with
      record
         Index         : Positive;
         Planet        : Hera.Planets.Planet_Type;
         Sector        : Hera.Sectors.Sector_Type;
         Government    : Hera.Corporations.Corporation_Type;
         Hazard_Level  : Unit_Real;
         Founded       : Hera.Calendar.Time;
         Market_Size   : Natural;
         Market        : Hera.Markets.Market_Type;
         Pops          : Pop_Lists.List;
         Warehouses    : Warehouse_Lists.List;
      end record;

   procedure Add_Initial_Population
     (Colony   : Root_Colony_Type'Class;
      Class    : Hera.Pops.Classes.Pop_Class_Type;
      Quantity : Hera.Quantities.Quantity_Type);

   procedure Add_Initial_Stock
     (Colony    : Root_Colony_Type'Class;
      Owner     : Hera.Corporations.Corporation_Type;
      Commodity : Hera.Commodities.Commodity_Type;
      Quantity  : Hera.Quantities.Quantity_Type);

   function Next_Index return Positive;

   function New_Colony
     (Colony : Root_Colony_Type'Class;
      Name   : String)
      return Colony_Type;

   function Government
     (Colony      : Root_Colony_Type'Class)
      return Hera.Corporations.Corporation_Type
   is (Colony.Government);

   function Market
     (Colony      : Root_Colony_Type'Class)
      return Hera.Markets.Market_Type
   is (Colony.Market);

   function Planet
     (Colony      : Root_Colony_Type'Class)
      return Hera.Planets.Planet_Type
   is (Colony.Planet);

   function Sector
     (Colony      : Root_Colony_Type'Class)
      return Hera.Sectors.Sector_Type
   is (Colony.Sector);

end Hera.Colonies;
