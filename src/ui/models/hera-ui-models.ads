private with Hera.Star_Systems;

with Hera.Json;

package Hera.UI.Models is

   type Root_Hera_Model is abstract tagged private;

   type Hera_Model is access all Root_Hera_Model'Class;

   function Name (Model : Root_Hera_Model) return String
                  is abstract;

   function Default_View_Name
     (Model : Root_Hera_Model)
      return String
      is abstract;

   function Account
     (Model : Root_Hera_Model'Class)
      return Hera.UI.UI_Account;

   procedure Start
     (Model     : in out Root_Hera_Model;
      User      : Hera.UI.UI_Account;
      Arguments : String);

   function Changed
     (Model : Root_Hera_Model)
      return Boolean
      is abstract;

   procedure Update
     (Model : in out Root_Hera_Model)
   is null;

   function Handle
     (Model   : in out Root_Hera_Model;
      State   : in out State_Interface'Class;
      Client  : Client_Id;
      Request : Hera.Json.Json_Value'Class)
      return Hera.Json.Json_Value'Class
   is abstract;

   function Get
     (Model   : Root_Hera_Model;
      State   : State_Interface'Class;
      Client  : Client_Id;
      Request : Hera.Json.Json_Value'Class)
      return Hera.Json.Json_Value'Class
      is abstract;

   function Error
     (Model   : Root_Hera_Model'class;
      State   : State_Interface'Class;
      Client  : Client_Id;
      Request : Hera.Json.Json_Value'Class;
      Message : String)
      return Hera.Json.Json_Value'Class;

   procedure Close
     (Model : in out Hera_Model);

private

   type Root_Hera_Model is abstract tagged
      record
         Account : Hera.UI.UI_Account;
      end record;

   function Account
     (Model : Root_Hera_Model'Class)
      return Hera.UI.UI_Account
   is (Model.Account);

--     type Detail_Level is (Low, Medium, High);

--     function Serialize
--       (Object     : not null access constant
--          Hera.Star_Systems.Root_Star_System_Entity'Class;
--        Parameters : Json.Json_Value'Class)
--        return Json.Json_Value'Class;

end Hera.UI.Models;
