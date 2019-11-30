private with Hera.Handles.Ship;

package Hera.Managers.Ships is

   function Default_Trade_Manager
     (Managed : Hera.Db.Managed_Reference)
      return Manager_Type;

private

   type Root_Ship_Manager is
     abstract new Root_Manager_Type with
      record
         Ship      : Hera.Handles.Ship.Ship_Handle;
         Owner     : Hera.Db.Agent_Reference;
         Has_Stock : Hera.Db.Has_Stock_Reference;
      end record;

end Hera.Managers.Ships;
