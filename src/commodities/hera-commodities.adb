with WL.String_Maps;

package body Hera.Commodities is

   package Commodity_Maps is
     new WL.String_Maps (Commodity_Type);

   package Commodity_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Commodity_Type);

   Map : Commodity_Maps.Map;
   List : Commodity_Lists.List;

   type Stock_Update is
     new Hera.Objects.Root_Update_Type with
      record
         Commodity : Commodity_Type;
         Stock     : Stock_Entry;
      end record;

   overriding procedure Execute
     (Update : Stock_Update;
      Object : in out Hera.Objects.Root_Hera_Object'class);

   overriding function Description
     (Update : Stock_Update)
      return String
   is ("set " & Update.Commodity.Tag & " quantity to "
       & Hera.Quantities.Show (Update.Stock.Quantity));

   ---------
   -- Add --
   ---------

   procedure Add
     (Has_Stock : Has_Stock_Interface'Class;
      Commodity : Commodity_Type;
      Quantity  : Hera.Quantities.Quantity_Type;
      Value     : Hera.Money.Money_Type)
   is
      use Hera.Money, Hera.Quantities;
      Stock : Stock_Entry := Has_Stock.Get_Stock (Commodity);
   begin
      Stock.Quantity := Stock.Quantity + Quantity;
      Stock.Value := Stock.Value + Value;
      Has_Stock.Set_Stock (Commodity, Stock);
   end Add;

   ---------
   -- Add --
   ---------

   procedure Add
     (Has_Stock : Has_Stock_Interface'Class;
      Commodity : Commodity_Type;
      Quantity  : Hera.Quantities.Quantity_Type;
      Price     : Hera.Money.Price_Type)
   is
   begin
      Has_Stock.Add (Commodity, Quantity, Hera.Money.Total (Price, Quantity));
   end Add;

   ----------------
   -- Complexity --
   ----------------

   function Complexity
     (Commodity : Root_Commodity_Type'Class)
      return Hera.Quantities.Quantity_Type
   is
      use Hera.Quantities;
      Base_Complexity : constant Quantity_Type :=
        (if Commodity.Class = Service_Token
         then To_Quantity (100.0 * Real (Commodity.Service_Quality ** 2))
         else Unit);
   begin
      return Result : Quantity_Type := Base_Complexity do
         for Item of Commodity.Components loop
            Result := Result
              + Item.Quantity * Item.Commodity.Complexity + Unit;
         end loop;
      end return;
   end Complexity;

   ----------------
   -- Components --
   ----------------

   function Components
     (Commodity : Root_Commodity_Type)
      return Commodity_Array
   is
      use Commodity_Component_Lists;
      Position : Cursor := Commodity.Components.First;
   begin
      return Arr : Commodity_Array
        (1 .. Natural (Commodity.Components.Length))
      do
         for I in Arr'Range loop
            Arr (I) := Element (Position).Commodity;
            Next (Position);
         end loop;
      end return;
   end Components;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Update : Stock_Update;
      Object : in out Hera.Objects.Root_Hera_Object'class)
   is
   begin
      Has_Stock_Interface'Class (Object).Update_Stock
        (Update.Commodity, Update.Stock);
   end Execute;

   ------------
   -- Exists --
   ------------

   function Exists (Tag : String) return Boolean is
   begin
      return Map.Contains (Tag);
   end Exists;

   ---------
   -- Get --
   ---------

   function Get (Tag : String) return Commodity_Type is
   begin
      return Map.Element (Tag);
   end Get;

   ---------------
   -- Get_Class --
   ---------------

   function Get_Class (Class : Commodity_Class) return Commodity_Array is
      Result : Commodity_Array (1 .. Natural (Map.Length));
      Count  : Natural := 0;
   begin
      for Commodity of Map loop
         if Commodity.Class = Class then
            Count := Count + 1;
            Result (Count) := Commodity;
         end if;
      end loop;
      pragma Assert (Result'Length > 0);
      return Result (1 .. Count);
   end Get_Class;

   ---------------
   -- Get_Stock --
   ---------------

   overriding function Get_Stock
     (From      : Stock_List;
      Commodity : Commodity_Type)
      return Stock_Entry
   is
   begin
      for Item of From.Stock loop
         if Item.Commodity = Commodity then
            return Item.Stock;
         end if;
      end loop;
      return (Hera.Quantities.Zero, Hera.Money.Zero);
   end Get_Stock;

   ---------------------------
   -- Initialize_Stock_List --
   ---------------------------

   procedure Initialize_Stock_List
     (List       : in out Stock_List'Class;
      Identifier : Hera.Identifiers.Object_Identifier)
   is
   begin
      List.Id := Identifier;
   end Initialize_Stock_List;

   ----------
   -- Load --
   ----------

   overriding procedure Load
     (Commodity : in out Root_Commodity_Type;
      From      : Tropos.Configuration)
   is
   begin
      Hera.Objects.Root_Localised_Object (Commodity).Load (From);
   end Load;

   ----------
   -- Load --
   ----------

   overriding procedure Load
     (Stock : in out Stock_List;
      From  : Tropos.Configuration)
   is
   begin
      for Config of From loop
         Stock.Set_Stock
           (Get (Config.Config_Name),
            (Hera.Quantities.Value (Config.Get ("quantity")),
             Hera.Money.Value (Config.Get ("value"))));
      end loop;
   end Load;

   -------------------
   -- New_Commodity --
   -------------------

   procedure New_Commodity
     (Commodity : Commodity_Type)
   is
   begin
      Map.Insert (Commodity.Tag, Commodity);
      List.Append (Commodity);
   end New_Commodity;

   -----------
   -- Price --
   -----------

   function Price
     (Has_Stock : Has_Stock_Interface'Class;
      Commodity : Commodity_Type)
      return Hera.Money.Price_Type
   is
      Stock : constant Stock_Entry :=
        Has_Stock.Get_Stock (Commodity);
   begin
      return Hera.Money.Price (Stock.Value, Stock.Quantity);
   end Price;

   -------------------
   -- Quality_Goods --
   -------------------

   function Quality_Goods
     (Goods   : Commodity_Array;
      Quality : Quality_Type)
      return Commodity_Array
   is
      Result : Commodity_Array (1 .. Goods'Length);
      Count  : Natural := 0;
   begin
      for Item of Goods loop
         if Item.Quality = Quality then
            Count := Count + 1;
            Result (Count) := Item;
         end if;
      end loop;
      return Result (1 .. Count);
   end Quality_Goods;

   --------------
   -- Quantity --
   --------------

   function Quantity
     (Commodity : Root_Commodity_Type;
      Component : Commodity_Type)
      return Hera.Quantities.Quantity_Type
   is
   begin
      for Item of Commodity.Components loop
         if Item.Commodity = Component then
            return Item.Quantity;
         end if;
      end loop;
      return Hera.Quantities.Zero;
   end Quantity;

   --------------
   -- Quantity --
   --------------

   function Quantity
     (Has_Stock : Has_Stock_Interface'Class;
      Commodity : Commodity_Type)
      return Hera.Quantities.Quantity_Type
   is
   begin
      return Has_Stock.Get_Stock (Commodity).Quantity;
   end Quantity;

   ------------
   -- Remove --
   ------------

   procedure Remove
     (Has_Stock : Has_Stock_Interface'Class;
      Commodity : Commodity_Type;
      Quantity  : Hera.Quantities.Quantity_Type;
      Value     : out Hera.Money.Money_Type)
   is
      use Hera.Money, Hera.Quantities;
      Stock : Stock_Entry := Has_Stock.Get_Stock (Commodity);
   begin
      Value :=
        (if Quantity = Stock.Quantity
         then Stock.Value
         else Total (Price (Stock.Value, Stock.Quantity), Quantity));
      Stock.Quantity := Stock.Quantity - Quantity;
      Stock.Value := Stock.Value - Value;
      Has_Stock.Set_Stock (Commodity, Stock);
   end Remove;

   ------------
   -- Remove --
   ------------

   procedure Remove
     (Has_Stock : Has_Stock_Interface'Class;
      Commodity : Commodity_Type;
      Quantity  : Hera.Quantities.Quantity_Type)
   is
      Value : Hera.Money.Money_Type with Unreferenced;
   begin
      Has_Stock.Remove (Commodity, Quantity, Value);
   end Remove;

   -------------
   -- Replace --
   -------------

   procedure Replace
     (To   : in out Has_Stock_Interface'Class;
      From : Has_Stock_Interface'Class)
   is
      procedure Do_Copy (Commodity : Commodity_Type;
                         Stock     : Stock_Entry);

      -------------
      -- Do_Copy --
      -------------

      procedure Do_Copy (Commodity : Commodity_Type;
                         Stock     : Stock_Entry)
      is
      begin
         To.Update_Stock (Commodity, (Stock.Quantity, Stock.Value));
      end Do_Copy;

   begin
      From.Scan_Stock (Do_Copy'Access);
   end Replace;

   ----------
   -- Save --
   ----------

   overriding procedure Save
     (Commodity : Root_Commodity_Type;
      To        : in out Tropos.Configuration)
   is
   begin
      Hera.Objects.Root_Localised_Object (Commodity).Save (To);
   end Save;

   ----------
   -- Save --
   ----------

   overriding procedure Save
     (Stock     : Stock_List;
      To        : in out Tropos.Configuration)
   is
      use Hera.Quantities;
   begin
      for Item of Stock.Stock loop
         if Item.Stock.Quantity > Zero then
            declare
               Config   : Tropos.Configuration :=
                 Tropos.New_Config (Item.Commodity.Tag);
            begin
               Config.Add
                 ("quantity", Hera.Quantities.Image (Item.Stock.Quantity));
               Config.Add
                 ("value", Hera.Money.Image (Item.Stock.Value));
               To.Add (Config);
            end;
         end if;
      end loop;
   end Save;

   ----------------
   -- Scan_Stock --
   ----------------

   overriding procedure Scan_Stock
     (List    : Stock_List;
      Process : not null access
        procedure (Commodity : Commodity_Type;
                   Stock : Stock_Entry))
   is
      use Hera.Quantities;
   begin
      for Item of List.Stock loop
         if Item.Stock.Quantity > Zero then
            Process (Item.Commodity, Item.Stock);
         end if;
      end loop;
   end Scan_Stock;

   ---------------------
   -- Scan_Stock_Join --
   ---------------------

   procedure Scan_Stock_Join
     (Left, Right : Has_Stock_Interface'Class;
      Process     : not null access
        procedure (Commodity : Hera.Commodities.Commodity_Type;
                   Left_Stock, Right_Stock : Stock_Entry))
   is
      use Hera.Quantities;
   begin
      for Commodity of List loop
         declare
            Left_Stock : constant Stock_Entry :=
              Left.Get_Stock (Commodity);
            Right_Stock : constant Stock_Entry :=
              Right.Get_Stock (Commodity);
         begin
            if Left_Stock.Quantity > Zero
              or else Right_Stock.Quantity > Zero
            then
               Process (Commodity, Left_Stock, Right_Stock);
            end if;
         end;
      end loop;
   end Scan_Stock_Join;

   ---------------
   -- Set_Stock --
   ---------------

   overriding procedure Set_Stock
     (List      : Stock_List;
      Commodity : Commodity_Type;
      Stock     : Stock_Entry)
   is
   begin
      Hera.Objects.Add_Update
        (Target => List.Id,
         Update =>
           (Stock_Update'
                (Hera.Objects.Root_Update_Type with
                     Commodity => Commodity,
                 Stock     => Stock)));
   end Set_Stock;

   -----------------------
   -- Update_Stock_List --
   -----------------------

   overriding procedure Update_Stock
     (List      : in out Stock_List;
      Commodity : Commodity_Type;
      Stock     : Stock_Entry)
   is
   begin
      for Item of List.Stock loop
         if Item.Commodity = Commodity then
            Item.Stock := Stock;
            return;
         end if;
      end loop;
      List.Stock.Append ((Commodity, Stock));
   end Update_Stock;

end Hera.Commodities;
