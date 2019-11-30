with WL.String_Maps;

package body Hera.Pops.Classes is

   package Pop_Class_Maps is
     new WL.String_Maps (Pop_Class_Type);

   Map : Pop_Class_Maps.Map;

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

   function Get (Tag : String) return Pop_Class_Type is
   begin
      return Map.Element (Tag);
   end Get;

   ----------------------------
   -- Iterate_Required_Goods --
   ----------------------------

   procedure Iterate_Required_Goods
     (Pop_Class : Root_Pop_Class_Object'Class;
      Process   : not null access
        procedure (Commodity : Hera.Commodities.Commodity_Type))
   is
   begin
      for Item of Pop_Class.Required_Goods loop
         Process (Item);
      end loop;
   end Iterate_Required_Goods;

   -------------------
   -- New_Pop_Class --
   -------------------

   procedure New_Pop_Class
     (Pop_Class : Pop_Class_Type)
   is
   begin
      Map.Insert (Pop_Class.Tag, Pop_Class);
   end New_Pop_Class;

end Hera.Pops.Classes;
