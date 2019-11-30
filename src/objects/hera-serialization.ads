private with Ada.Containers.Indefinite_Holders;

with Hera.Json;
with Hera.Knowledge;

with Hera.Money;

package Hera.Serialization is

   type Detail_Level_Type is (Low, Medium, High, Full);

   type Serializable_Interface is interface
     and Hera.Knowledge.Knowable_Interface;

   procedure Serialize
     (Item            : Serializable_Interface;
      Detail          : Detail_Level_Type;
      Knowledge       : Hera.Knowledge.Knowledge_Interface'Class;
      Json            : in out Hera.Json.Json_Object'Class)
   is abstract;

   procedure Set_Property
     (Item      : Serializable_Interface'Class;
      Target    : in out Hera.Json.Json_Object'Class;
      Knowledge : Hera.Knowledge.Knowledge_Interface'Class;
      Level     : Hera.Knowledge.Knowledge_Level_Type;
      Name      : String;
      Value     : String);

   procedure Set_Property
     (Item      : Serializable_Interface'Class;
      Target    : in out Hera.Json.Json_Object'Class;
      Knowledge : Hera.Knowledge.Knowledge_Interface'Class;
      Level     : Hera.Knowledge.Knowledge_Level_Type;
      Name      : String;
      Value     : Hera.Money.Money_Type);

   type Serializer_Type is tagged private;

   procedure Create_Serializer
     (Serializer   : in out Serializer_Type;
      Detail_Level : Detail_Level_Type;
      Knowledge    : Hera.Knowledge.Knowledge_Interface'Class);

   procedure Serialize
     (Serializer      : Serializer_Type;
      Item            : Serializable_Interface'Class;
      Target          : in out Json.Json_Object'Class);

private

   package Knowledge_Holders is
     new Ada.Containers.Indefinite_Holders
       (Hera.Knowledge.Knowledge_Interface'Class,
        Hera.Knowledge."=");

   type Serializer_Type is tagged
      record
         Detail_Level : Detail_Level_Type;
         Knowledge    : Knowledge_Holders.Holder;
      end record;

end Hera.Serialization;
