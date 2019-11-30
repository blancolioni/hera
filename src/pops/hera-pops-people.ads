with Hera.Objects;

with Hera.Accounts;

with Hera.Money;
with Hera.Quantities;

with Hera.Commodities;
with Hera.Markets;

with Hera.Pops.Classes;

limited with Hera.Colonies;

package Hera.Pops.People is

   type Root_Population_Object is
     new Hera.Objects.Root_Hera_Object
     and Hera.Accounts.Has_Account_Interface
     and Hera.Markets.Trader_Interface
     and Hera.Commodities.Has_Stock_Interface
   with private;

   function Class
     (Pop : Root_Population_Object'Class)
      return Hera.Pops.Classes.Pop_Class_Type;

   function Size
     (Pop : Root_Population_Object'Class)
      return Hera.Quantities.Quantity_Type;

   procedure Add_Population
     (Pop      : Root_Population_Object'Class;
      Quantity : Hera.Quantities.Quantity_Type);

   procedure Remove_Population
     (Pop      : Root_Population_Object'Class;
      Quantity : Hera.Quantities.Quantity_Type);

   procedure Move_Population
     (From        : Root_Population_Object'Class;
      To          : not null access constant Root_Population_Object'Class;
      Transaction : not null access constant
        Hera.Accounts.Root_Transaction_Type'Class;
      Quantity    : Hera.Quantities.Quantity_Type);

   type Population_Type is access constant Root_Population_Object'Class;

   function New_Population
     (Class  : Hera.Pops.Classes.Pop_Class_Type;
      Size   : Hera.Quantities.Quantity_Type;
      Cash   : Hera.Money.Money_Type;
      Colony : not null access constant Hera.Colonies.Root_Colony_Type'Class;
      Market : Hera.Markets.Market_Type)
      return Population_Type;

private

   type Pop_Colony is access constant Hera.Colonies.Root_Colony_Type'Class;

   type Root_Population_Object is
     new Hera.Objects.Root_Hera_Object
     and Hera.Accounts.Has_Account_Interface
     and Hera.Markets.Trader_Interface
     and Hera.Commodities.Has_Stock_Interface with
      record
         Colony    : Pop_Colony;
         Market    : Hera.Markets.Market_Type;
         Account   : Hera.Accounts.Account_Type;
         Class     : Hera.Pops.Classes.Pop_Class_Type;
         Size      : Hera.Quantities.Quantity_Type;
         Stock     : Hera.Commodities.Stock_List;
         Happiness : Unit_Real := 1.0;
      end record;

   overriding function Name
     (Pop : Root_Population_Object)
      return String
   is ("pop-" & String (Pop.Identifier));

   overriding function Account
     (Pop : Root_Population_Object)
      return Hera.Accounts.Account_Type
   is (Pop.Account);

   overriding procedure On_Buy
     (Pop         : Root_Population_Object;
      Commodity   : Hera.Commodities.Commodity_Type;
      Quantity    : Hera.Quantities.Quantity_Type;
      Total_Paid  : Hera.Money.Money_Type);

   overriding procedure On_Sell
     (Pop          : Root_Population_Object;
      Commodity    : Hera.Commodities.Commodity_Type;
      Quantity     : Hera.Quantities.Quantity_Type;
      Total_Earned : Hera.Money.Money_Type);

   overriding function Get_Stock
     (From      : Root_Population_Object;
      Commodity : Hera.Commodities.Commodity_Type)
      return Hera.Commodities.Stock_Entry;

   overriding procedure Set_Stock
     (To        : Root_Population_Object;
      Commodity : Hera.Commodities.Commodity_Type;
      Stock     : Hera.Commodities.Stock_Entry);

   overriding procedure Scan_Stock
     (List      : Root_Population_Object;
      Process   : not null access
        procedure (Commodity : Hera.Commodities.Commodity_Type;
                   Stock : Hera.Commodities.Stock_Entry));

   overriding procedure Update_Stock
     (Pop       : in out Root_Population_Object;
      Commodity : Hera.Commodities.Commodity_Type;
      Stock     : Hera.Commodities.Stock_Entry);

   procedure Update_Happiness
     (Pop           : Root_Population_Object'Class;
      New_Happiness : Unit_Real);

   function Class
     (Pop : Root_Population_Object'Class)
      return Hera.Pops.Classes.Pop_Class_Type
   is (Pop.Class);

   function Size
     (Pop : Root_Population_Object'Class)
      return Hera.Quantities.Quantity_Type
   is (Pop.Size);

end Hera.Pops.People;
