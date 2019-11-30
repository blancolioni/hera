with Hera.Quantities;

with Hera.Handles.Ship;

package Hera.Ships is

   subtype Ship_Handle is Hera.Handles.Ship.Ship_Handle;

   function Maximum_Cargo
     (Ship : Ship_Handle)
      return Hera.Quantities.Quantity_Type;

   function Maximum_Jump
     (Ship : Ship_Handle)
      return Non_Negative_Real;

end Hera.Ships;
