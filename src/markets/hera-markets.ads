private with Ada.Strings.Unbounded;
private with WL.Heaps;
private with Hera.Commodities.Maps;

with Hera.Money;
with Hera.Quantities;

with Hera.Objects;

with Hera.Accounts;
with Hera.Commodities;

package Hera.Markets is

   type Trader_Interface is interface
     and Hera.Accounts.Has_Account_Interface;

   function Name
     (Trader : Trader_Interface)
      return String
      is abstract;

   type Root_Trade_Node_Type is
     new Hera.Objects.Root_Named_Object
     and Hera.Commodities.Has_Stock_Interface
   with private;

   type Trade_Node_Type is access constant Root_Trade_Node_Type'Class;

   function New_Trade_Node
     (Name : String)
     return Trade_Node_Type;

   type Trade_Center_Type is private;

   procedure New_Trade_Center
     (Name        : String;
      Production  : Trade_Node_Type;
      Consumption : Trade_Node_Type);

   procedure Iterate_Trade_Centers
     (Process : not null access
        procedure (Name : String;
                   Production : Trade_Node_Type;
                   Consumption : Trade_Node_Type));

   type Root_Market_Type is
     new Hera.Objects.Root_Hera_Object with private;

   type Market_Type is access constant Root_Market_Type'Class;

   function Create_Market return Market_Type;

   procedure On_Buy
     (Trader     : Trader_Interface;
      Commodity  : Hera.Commodities.Commodity_Type;
      Quantity   : Hera.Quantities.Quantity_Type;
      Total_Paid : Hera.Money.Money_Type)
   is abstract;

   procedure On_Sell
     (Trader       : Trader_Interface;
      Commodity    : Hera.Commodities.Commodity_Type;
      Quantity     : Hera.Quantities.Quantity_Type;
      Total_Earned : Hera.Money.Money_Type)
   is abstract;

   type Transaction_Result is
      record
         Quantity : Hera.Quantities.Quantity_Type;
         Cost     : Hera.Money.Money_Type;
      end record;

   procedure Buy
     (From_Market : Root_Market_Type'Class;
      Trader      : not null access constant Trader_Interface'Class;
      Stock       : not null access constant
        Hera.Commodities.Has_Stock_Interface'Class;
      Commodity   : Hera.Commodities.Commodity_Type;
      Quantity    : Hera.Quantities.Quantity_Type;
      Cash        : Hera.Money.Money_Type);

   procedure Sell
     (To_Market : Root_Market_Type'Class;
      Trader    : not null access constant Trader_Interface'Class;
      Stock     : not null access constant
        Hera.Commodities.Has_Stock_Interface'Class;
      Commodity : Hera.Commodities.Commodity_Type;
      Quantity  : Hera.Quantities.Quantity_Type;
      Price     : Hera.Money.Price_Type);

   procedure Clear_Transients
     (Market : Root_Market_Type'Class);

private

   type Trader_Type is access constant Trader_Interface'Class;

   type Sell_Offer_Record is
      record
         Trader    : Trader_Type;
         Commodity : Hera.Commodities.Commodity_Type;
         Quantity  : Hera.Quantities.Quantity_Type;
         Price     : Hera.Money.Price_Type;
      end record;

   package Sell_Offer_Queues is
     new WL.Heaps
       (Key_Type     => Hera.Money.Price_Type,
        Element_Type => Sell_Offer_Record,
        "<"          => Hera.Money.">");

   package Sell_Offer_Maps is
     new Hera.Commodities.Maps (Sell_Offer_Queues.Heap, Sell_Offer_Queues."=");

   type Root_Market_Type is
     new Hera.Objects.Root_Hera_Object with
      record
         Sell_Offers : Sell_Offer_Maps.Map;
      end record;

   type Root_Trade_Node_Type is
     new Hera.Objects.Root_Named_Object
     and Hera.Commodities.Has_Stock_Interface with
      record
         Stock : Hera.Commodities.Stock_List;
      end record;

   overriding function Get_Stock
     (From      : Root_Trade_Node_Type;
      Commodity : Hera.Commodities.Commodity_Type)
      return Hera.Commodities.Stock_Entry;

   overriding procedure Set_Stock
     (To        : Root_Trade_Node_Type;
      Commodity : Hera.Commodities.Commodity_Type;
      Stock     : Hera.Commodities.Stock_Entry);

   overriding procedure Scan_Stock
     (List      : Root_Trade_Node_Type;
      Process   : not null access
        procedure (Commodity : Hera.Commodities.Commodity_Type;
                   Stock : Hera.Commodities.Stock_Entry));

   overriding procedure Update_Stock
     (Trade_Node : in out Root_Trade_Node_Type;
      Commodity  : Hera.Commodities.Commodity_Type;
      Stock      : Hera.Commodities.Stock_Entry);

   type Trade_Center_Type is
      record
         Name        : Ada.Strings.Unbounded.Unbounded_String;
         Production  : Trade_Node_Type;
         Consumption : Trade_Node_Type;
      end record;

end Hera.Markets;
