package Hera.Logging is

   procedure Log
     (Category : String;
      Message  : String);

   procedure Start_Logging;
   procedure Stop_Logging;

   procedure Start_Update;
   procedure Finish_Update;

end Hera.Logging;
