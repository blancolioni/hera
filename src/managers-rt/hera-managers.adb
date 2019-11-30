with Hera.Logging;
with Hera.Updates.Events;

package body Hera.Managers is

   --------------
   -- Activate --
   --------------

   overriding procedure Activate
     (Update : Manager_Update)
   is
      use type Hera.Db.Managed_Reference;
      Has_Managed : constant Boolean :=
        Update.Manager.Managed /= Hera.Db.Null_Managed_Reference;
   begin
      Update.Manager.Has_Next_Update := False;
      Update.Manager.Activate;
      Update.Manager.Signaled := False;

      if Update.Manager.Has_Next_Update then
         Update.Manager.Is_Active := True;
         Hera.Updates.Events.Update_At
           (Clock  => Update.Manager.Next_Update,
            Update => Update);
         if Has_Managed then
            Hera.Db.Managed.Update_Managed (Update.Manager.Managed)
              .Set_Next_Event (Update.Manager.Next_Update)
              .Set_Active (True)
              .Done;
         end if;
      else
         Hera.Logging.Log
           (Category => Update.Manager.Identifier,
            Message  => "deactivating");
         Update.Manager.Is_Active := False;
         if Has_Managed then
            Hera.Db.Managed.Update_Managed (Update.Manager.Managed)
              .Set_Active (False)
              .Done;
         end if;
      end if;

   end Activate;

   ----------------
   -- Active_Key --
   ----------------

   function Active_Key
     (Managed : Hera.Db.Managed_Reference)
      return String
   is
   begin
      return Hera.Db.To_String (Managed);
   end Active_Key;

   ---------
   -- Log --
   ---------

   procedure Log
     (Manager : Root_Manager_Type;
      Message : String)
   is
   begin
      Hera.Logging.Log
        (Category => Root_Manager_Type'Class (Manager).Identifier,
         Message  => Message);
   end Log;

   ---------------------------
   -- Set_Next_Update_Delay --
   ---------------------------

   procedure Set_Next_Update_Delay
     (Manager      : not null access Root_Manager_Type'Class;
      Update_Delay : Duration)
   is
      use type Hera.Calendar.Time;
   begin
      Manager.Set_Next_Update_Time (Hera.Calendar.Clock + Update_Delay);
   end Set_Next_Update_Delay;

   --------------------------
   -- Set_Next_Update_Time --
   --------------------------

   procedure Set_Next_Update_Time
     (Manager     : not null access Root_Manager_Type'Class;
      Update_Time : Hera.Calendar.Time)
   is
   begin
      if Manager.Is_Active then
         Manager.Has_Next_Update := True;
         Manager.Next_Update := Update_Time;
      else
         declare
            Update : constant Manager_Update :=
                       (Manager => Manager_Type (Manager));
         begin
            Manager.Is_Active := True;
            Hera.Updates.Events.Update_At
              (Clock  => Manager.Next_Update,
               Update => Update);
         end;
      end if;

   end Set_Next_Update_Time;

   ------------
   -- Signal --
   ------------

   procedure Signal
     (Managed : Hera.Db.Managed_Reference)
   is
   begin
      Active_Map.Element (Active_Key (Managed))
       .Set_Next_Update_Time (Hera.Calendar.Clock);
   end Signal;

end Hera.Managers;
