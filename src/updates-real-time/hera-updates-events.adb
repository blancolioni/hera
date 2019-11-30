with Hera.Updates.Tasks;

package body Hera.Updates.Events is

   ---------------
   -- Update_At --
   ---------------

   procedure Update_At
     (Clock  : Hera.Calendar.Time;
      Update : Update_Interface'Class)
   is
   begin
      Hera.Updates.Tasks.Update_Map.Add_Update (Clock, Update);
   end Update_At;

   -----------------------
   -- Update_Next_Cycle --
   -----------------------

   procedure Update_Next_Cycle
     (Update : Update_Interface'Class)
   is
   begin
      Update_With_Delay (Hera.Calendar.Days (Update_Cycle_Days), Update);
   end Update_Next_Cycle;

   -----------------------
   -- Update_With_Delay --
   -----------------------

   procedure Update_With_Delay
     (Wait   : Duration;
      Update : Update_Interface'Class)
   is
      use type Hera.Calendar.Time;
   begin
      Hera.Updates.Tasks.Update_Map.Add_Update
        (Hera.Calendar.Clock + Wait, Update);
   end Update_With_Delay;

end Hera.Updates.Events;
