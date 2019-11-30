package body Hera.Warehouses is

   Warehouse_Version : constant Hera.Objects.Object_Version := "0.0.1";

   ---------------
   -- Get_Stock --
   ---------------

   overriding function Get_Stock
     (From      : Root_Warehouse_Type;
      Commodity : Hera.Commodities.Commodity_Type)
      return Hera.Commodities.Stock_Entry
   is
   begin
      return From.Stock.Get_Stock (Commodity);
   end Get_Stock;

   -------------------
   -- New_Warehouse --
   -------------------

   function New_Warehouse
     (Owner : Hera.Corporations.Corporation_Type) return Warehouse_Type
   is
      Warehouse : Root_Warehouse_Type := Root_Warehouse_Type'
        (Hera.Objects.Root_Named_Object with
         Owner => Owner,
         Stock => <>);
   begin
      Warehouse.Initialize (Warehouse_Version);
      Warehouse.Stock.Initialize_Stock_List (Warehouse.Identifier);
      return Warehouse_Type (Warehouse.Save_Object);
   end New_Warehouse;

   ----------------
   -- Scan_Stock --
   ----------------

   overriding procedure Scan_Stock
     (List      : Root_Warehouse_Type;
      Process   : not null access
        procedure (Commodity : Hera.Commodities.Commodity_Type;
                   Stock : Hera.Commodities.Stock_Entry))
   is
   begin
      List.Stock.Scan_Stock (Process);
   end Scan_Stock;

   ---------------
   -- Set_Stock --
   ---------------

   overriding procedure Set_Stock
     (To        : Root_Warehouse_Type;
      Commodity : Hera.Commodities.Commodity_Type;
      Stock     : Hera.Commodities.Stock_Entry)
   is
   begin
      To.Stock.Set_Stock (Commodity, Stock);
   end Set_Stock;

   ------------------
   -- Update_Stock --
   ------------------

   overriding procedure Update_Stock
     (Warehouse : in out Root_Warehouse_Type;
      Commodity : Hera.Commodities.Commodity_Type;
      Stock     : Hera.Commodities.Stock_Entry)
   is
   begin
      Warehouse.Stock.Update_Stock (Commodity, Stock);
   end Update_Stock;

end Hera.Warehouses;
