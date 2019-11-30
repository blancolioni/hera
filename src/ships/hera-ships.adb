with Hera.Db.Component;
with Hera.Db.Ship_Component;

package body Hera.Ships is

   -------------------
   -- Maximum_Cargo --
   -------------------

   function Maximum_Cargo
     (Ship : Ship_Handle) return Hera.Quantities.Quantity_Type
   is
      Space : Non_Negative_Real := Ship.Ship_Design.Capacity;
   begin
      for Component of
        Hera.Db.Ship_Component.Select_By_Ship (Ship.Reference)
      loop
         Space := Space
           - Hera.Db.Component.Get (Component.Component).Mass;
      end loop;
      return Hera.Quantities.To_Quantity (Space);
   end Maximum_Cargo;

   ------------------
   -- Maximum_Jump --
   ------------------

   function Maximum_Jump (Ship : Ship_Handle) return Non_Negative_Real is
      pragma Unreferenced (Ship);
   begin
      return 7.0;
   end Maximum_Jump;

end Hera.Ships;
