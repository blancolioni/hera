with AWS.Response;
with AWS.Status;

private package Hera.UI.Web_UI.Logging is

   procedure Log_Request
     (Request  : AWS.Status.Data;
      Response : AWS.Response.Data);

   procedure On_Starting;

   procedure On_Stopping (Message : String);
   procedure On_Stop;

end Hera.UI.Web_UI.Logging;
