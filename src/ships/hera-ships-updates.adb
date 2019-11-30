with Hera.Calendar;
with Hera.Logging;
with Hera.Managers;
with Hera.Random;

with Hera.Star_Systems;

with Hera.Updates.Events;

package body Hera.Ships.Updates is

   type Ship_Update is new Hera.Updates.Update_Interface with
      record
         Ship : Hera.Handles.Ship.Ship_Handle;
      end record;

   overriding procedure Activate (Update : Ship_Update);

   --------------
   -- Activate --
   --------------

   overriding procedure Activate (Update : Ship_Update) is
      use Hera.Calendar;
      use all type Hera.Db.Ship_Status;
   begin

      Hera.Logging.Log
        (Update.Ship.Name,
         "updating; current status "
         & Hera.Db.Ship_Status'Image (Update.Ship.Status));

      case Update.Ship.Status is
         when Idle =>
            Hera.Managers.Signal
              (Update.Ship.Reference_Managed);
         when Trading =>
            null;
         when Leaving =>
            declare
               Distance : constant Non_Negative_Real :=
                 Hera.Star_Systems.Distance
                   (Update.Ship.Star_System,
                    Update.Ship.Destination);
            begin
               Update.Ship.Update
                 .Set_Status (Jumping)
                 .Set_Next_Status (Hera.Calendar.Clock + Days (Distance))
                 .Set_Star_System (Hera.Db.Null_Star_System_Reference)
                 .Done;
               Hera.Updates.Events.Update_At
                 (Clock  => Update.Ship.Next_Status,
                  Update => Update);
            end;

         when Jumping =>
            Update.Ship.Update
              .Set_Status (Arriving)
              .Set_Next_Status
                (Hera.Calendar.Clock
                 + Days (Hera.Random.Unit_Random + 0.5))
              .Set_Star_System (Update.Ship.Destination.Reference_Star_System)
              .Set_Destination (Hera.Db.Null_Star_System_Reference)
              .Done;
            Hera.Updates.Events.Update_At
              (Clock  => Update.Ship.Next_Status,
               Update => Update);
         when Arriving =>
            Update.Ship.Update
              .Set_Status (Idle)
              .Done;
            Hera.Managers.Signal
              (Update.Ship.Reference_Managed);
      end case;
   end Activate;

   ------------
   -- Signal --
   ------------

   procedure Signal (Ship : Hera.Db.Ship_Reference) is
   begin
      Hera.Updates.Events.Update_At
        (Clock  => Hera.Calendar.Clock,
         Update => Ship_Update'(Ship => Hera.Handles.Ship.Get (Ship)));
   end Signal;

end Hera.Ships.Updates;
