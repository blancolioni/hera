with Hera.Objects;

with Hera.Commodities;
with Hera.Corporations;

package Hera.Warehouses is

   type Root_Warehouse_Type is
     new Hera.Objects.Root_Named_Object
     and Hera.Commodities.Has_Stock_Interface
   with private;

   function Owner
     (Warehouse : Root_Warehouse_Type)
      return Hera.Corporations.Corporation_Type;

   type Warehouse_Type is access constant Root_Warehouse_Type'Class;

   function New_Warehouse
     (Owner : Hera.Corporations.Corporation_Type)
      return Warehouse_Type;

private

   type Root_Warehouse_Type is
     new Hera.Objects.Root_Named_Object
     and Hera.Commodities.Has_Stock_Interface with
      record
         Owner : Hera.Corporations.Corporation_Type;
         Stock : Hera.Commodities.Stock_List;
      end record;

   overriding function Get_Stock
     (From      : Root_Warehouse_Type;
      Commodity : Hera.Commodities.Commodity_Type)
      return Hera.Commodities.Stock_Entry;

   overriding procedure Set_Stock
     (To        : Root_Warehouse_Type;
      Commodity : Hera.Commodities.Commodity_Type;
      Stock     : Hera.Commodities.Stock_Entry);

   overriding procedure Scan_Stock
     (List      : Root_Warehouse_Type;
      Process   : not null access
        procedure (Commodity : Hera.Commodities.Commodity_Type;
                   Stock : Hera.Commodities.Stock_Entry));

   overriding procedure Update_Stock
     (Warehouse : in out Root_Warehouse_Type;
      Commodity : Hera.Commodities.Commodity_Type;
      Stock     : Hera.Commodities.Stock_Entry);

   function Owner
     (Warehouse : Root_Warehouse_Type)
      return Hera.Corporations.Corporation_Type
   is (Warehouse.Owner);

end Hera.Warehouses;
