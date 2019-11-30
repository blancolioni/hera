package Hera.Updates is

   Update_Cycle_Days : constant Non_Negative_Real := 10.0;

   type Update_Interface is interface;

   procedure Activate
     (Update : Update_Interface)
   is abstract;

end Hera.Updates;
