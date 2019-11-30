package Hera.Knowledge is

   type Knowledge_Level_Type is
     (None,
      Exists,
      Full);

   type Knowable_Interface is interface;

   type Knowledge_Interface is interface;

   function Knowledge_Level
     (Knowledge : Knowledge_Interface;
      Object    : Knowable_Interface'Class)
      return Knowledge_Level_Type
      is abstract;

   function Knows
     (Knowledge : Knowledge_Interface'Class;
      Object    : Knowable_Interface'Class;
      Level     : Knowledge_Level_Type)
      return Boolean;

   function Knows
     (Knowledge : Knowledge_Interface'Class;
      Object    : not null access constant Knowable_Interface'Class;
      Level     : Knowledge_Level_Type)
      return Boolean;

   --     procedure Update
--       (Target : in out Knowable_Interface'Class;
--        Knowledge : Knowledge_Interface'Class;
--        Level     : Knowledge_Level_Type;
--        Update    : not null access
--          procedure (Target : in out Knowable_Interface'Class));

private

   function Knows
     (Knowledge : Knowledge_Interface'Class;
      Object    : Knowable_Interface'Class;
      Level     : Knowledge_Level_Type)
      return Boolean
   is (Knowledge.Knowledge_Level (Object) >= Level);

   function Knows
     (Knowledge : Knowledge_Interface'Class;
      Object    : not null access constant Knowable_Interface'Class;
      Level     : Knowledge_Level_Type)
      return Boolean
   is (Knowledge.Knows (Object.all, Level));

end Hera.Knowledge;
