with Hera.Json;

package Hera.UI.Web_UI.Handlers.Clients is

   type New_Client_Handler is
     new Routes.Request_Handler with private;

   type Client_Request_Handler is
     new Routes.Request_Handler with private;

private

   type New_Client_Handler is
     new Routes.Request_Handler with null record;

   overriding function Handle_Post
     (Handler    : New_Client_Handler;
      State      : in out State_Interface'Class;
      Parameters : Routes.Parameter_Container'Class)
      return Hera.Json.Json_Value'Class;

   type Client_Request_Handler is
     new Routes.Request_Handler with null record;

   overriding function Handle_Post
     (Handler    : Client_Request_Handler;
      State      : in out State_Interface'Class;
      Parameters : Routes.Parameter_Container'Class)
      return Hera.Json.Json_Value'Class;

   overriding function Handle_Get
     (Handler    : Client_Request_Handler;
      State      : State_Interface'Class;
      Parameters : Routes.Parameter_Container'Class)
      return Hera.Json.Json_Value'Class;

end Hera.UI.Web_UI.Handlers.Clients;
