package body Hera.Serialization is

   -----------------------
   -- Create_Serializer --
   -----------------------

   procedure Create_Serializer
     (Serializer   : in out Serializer_Type;
      Detail_Level : Detail_Level_Type;
      Knowledge    : Hera.Knowledge.Knowledge_Interface'Class)
   is
   begin
      Serializer := Serializer_Type'
        (Detail_Level => Detail_Level,
         Knowledge    => Knowledge_Holders.To_Holder (Knowledge));
   end Create_Serializer;

   ---------------
   -- Serialize --
   ---------------

   procedure Serialize
     (Serializer      : Serializer_Type;
      Item            : Serializable_Interface'Class;
      Target          : in out Json.Json_Object'Class)
   is
   begin
      Item.Serialize
        (Detail    => Serializer.Detail_Level,
         Knowledge => Serializer.Knowledge.Element,
         Json      => Target);
   end Serialize;

   ------------------
   -- Set_Property --
   ------------------

   procedure Set_Property
     (Item      :        Serializable_Interface'Class;
      Target    : in out Hera.Json.Json_Object'Class;
      Knowledge :        Hera.Knowledge.Knowledge_Interface'Class;
      Level     :        Hera.Knowledge.Knowledge_Level_Type;
      Name      :        String;
      Value     :        String)
   is
   begin
      if Knowledge.Knows (Item, Level) then
         Target.Set_Property (Name, Value);
      end if;
   end Set_Property;

   ------------------
   -- Set_Property --
   ------------------

   procedure Set_Property
     (Item      :        Serializable_Interface'Class;
      Target    : in out Hera.Json.Json_Object'Class;
      Knowledge :        Hera.Knowledge.Knowledge_Interface'Class;
      Level     :        Hera.Knowledge.Knowledge_Level_Type;
      Name      :        String;
      Value     :        Hera.Money.Money_Type)
   is
   begin
      Item.Set_Property (Target, Knowledge, Level,
                         Name, Hera.Money.Image (Value));
   end Set_Property;

end Hera.Serialization;
