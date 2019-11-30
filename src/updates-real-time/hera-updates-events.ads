with Hera.Calendar;

package Hera.Updates.Events is

   procedure Update_At
     (Clock  : Hera.Calendar.Time;
      Update : Update_Interface'Class);

   procedure Update_With_Delay
     (Wait   : Duration;
      Update : Update_Interface'Class);

   procedure Update_Next_Cycle
     (Update : Update_Interface'Class);

end Hera.Updates.Events;
