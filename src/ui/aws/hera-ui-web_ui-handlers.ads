with Hera.UI.Web_UI.Routes;

package Hera.UI.Web_UI.Handlers is

   Handler_Error : exception;

   function Handle_Login
     return Routes.Request_Handler'Class;

   function Handle_New_Client
     return Routes.Request_Handler'Class;

   function Handle_Environment_Request
     return Routes.Request_Handler'Class;

   function Handle_Client_Request
     return Routes.Request_Handler'Class;

   function Handle_Status_Request
     return Routes.Request_Handler'Class;

end Hera.UI.Web_UI.Handlers;
