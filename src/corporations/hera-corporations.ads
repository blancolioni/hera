private with Ada.Containers.Vectors;
private with Hera.Objects.Knowledge;

with Tropos;

with Hera.Objects;
with Hera.Knowledge;

with Hera.Accounts;
with Hera.Commodities;
with Hera.Markets;
with Hera.Planets;

limited with Hera.Colonies;

with Hera.Money;
with Hera.Quantities;

package Hera.Corporations is

   type Root_Corporation_Type is
     new Hera.Objects.Root_Named_Object
     and Hera.Accounts.Has_Account_Interface
     and Hera.Markets.Trader_Interface
     and Hera.Knowledge.Knowledge_Interface
   with private;

   type Corporation_Type is access constant Root_Corporation_Type'Class;

   function Home_Planet
     (Corporation : Root_Corporation_Type'Class)
      return Hera.Planets.Planet_Type;

   function Exists
     (Name : String)
      return Boolean;

   function Get
     (Name : String)
      return Corporation_Type
     with Pre => Exists (Name);

   type Corporate_Colony is access constant
     Hera.Colonies.Root_Colony_Type'Class;

   type Corporate_Colony_Array is
     array (Positive range <>) of Corporate_Colony;

   procedure Add_Colony
     (Corporation : Root_Corporation_Type'Class;
      Colony      : not null access constant
        Hera.Colonies.Root_Colony_Type'Class);

   procedure Update_Knowledge
     (Corporation     : Root_Corporation_Type'Class;
      Object          : not null access constant
        Hera.Objects.Root_Hera_Object'Class;
      Knowledge_Level : Hera.Knowledge.Knowledge_Level_Type);

   function Colonies
     (Corporation : Root_Corporation_Type'Class)
      return Corporate_Colony_Array;

   type Corporation_Array is
     array (Positive range <>) of Corporation_Type;

   procedure Save (Config : in out Tropos.Configuration);

   procedure Iterate
     (Process : not null access
        procedure (Corporation : Corporation_Type));

private

   package Corporate_Colony_Vectors is
     new Ada.Containers.Vectors (Positive, Corporate_Colony);

   type Root_Corporation_Type is
     new Hera.Objects.Root_Named_Object
     and Hera.Accounts.Has_Account_Interface
     and Hera.Markets.Trader_Interface
     and Hera.Knowledge.Knowledge_Interface with
      record
         Home_Planet : Hera.Planets.Planet_Type;
         Account     : Hera.Accounts.Account_Type;
         Autoplayer  : Boolean;
         Colonies    : Corporate_Colony_Vectors.Vector;
         Knowledge   : Hera.Objects.Knowledge.Object_Knowledge_Type;
      end record;

   overriding function Account
     (Corporation : Root_Corporation_Type)
      return Hera.Accounts.Account_Type
   is (Corporation.Account);

   overriding procedure On_Buy
     (Corporation : Root_Corporation_Type;
      Commodity   : Hera.Commodities.Commodity_Type;
      Quantity    : Hera.Quantities.Quantity_Type;
      Total_Paid  : Hera.Money.Money_Type);

   overriding procedure On_Sell
     (Corporation  : Root_Corporation_Type;
      Commodity    : Hera.Commodities.Commodity_Type;
      Quantity     : Hera.Quantities.Quantity_Type;
      Total_Earned : Hera.Money.Money_Type);

   overriding function Knowledge_Level
     (Corporation : Root_Corporation_Type;
      Object      : Hera.Knowledge.Knowable_Interface'Class)
      return Hera.Knowledge.Knowledge_Level_Type;

   function Home_Planet
     (Corporation : Root_Corporation_Type'Class)
      return Hera.Planets.Planet_Type
   is (Corporation.Home_Planet);

   function New_Corporation
     (Name       : String;
      Cash       : Hera.Money.Money_Type;
      Planet     : Hera.Planets.Planet_Type;
      Autoplayer : Boolean)
      return Corporation_Type;

end Hera.Corporations;
