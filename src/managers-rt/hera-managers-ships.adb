with Hera.Managers.Ships.Trade;

with Hera.Db.Ship;

package body Hera.Managers.Ships is

   ---------------------------
   -- Default_Trade_Manager --
   ---------------------------

   function Default_Trade_Manager
     (Managed : Hera.Db.Managed_Reference)
      return Manager_Type
   is
   begin
      return Trade.Trade_Manager
        (Ship => Hera.Handles.Ship.Get
           (Hera.Db.Ship.Get_Ship (Managed).Get_Ship_Reference));
   end Default_Trade_Manager;

end Hera.Managers.Ships;
