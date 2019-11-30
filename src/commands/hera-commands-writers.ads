private with Ada.Strings.Unbounded;

with Hera.Json;
with Hera.Writers;

package Hera.Commands.Writers is

   type String_Writer is
     new Hera.Writers.Writer_Interface with private;

   overriding procedure Put
     (Writer : in out String_Writer;
      Text   : String);

   overriding procedure New_Line
     (Writer : in out String_Writer);

   overriding procedure Put_Error
     (Writer  : in out String_Writer;
      Message : String);

   function To_String (Writer : String_Writer) return String;

   type Json_Writer is
     new Hera.Writers.Writer_Interface with private;

   overriding procedure Put
     (Writer : in out Json_Writer;
      Text   : String);

   overriding procedure New_Line
     (Writer : in out Json_Writer);

   overriding procedure Put_Error
     (Writer  : in out Json_Writer;
      Message : String);

   overriding procedure Control
     (Writer : in out Json_Writer;
      Packet : Hera.Json.Json_Value'Class);

   overriding procedure Return_Value
     (Writer : in out Json_Writer;
      Value  : Hera.Json.Json_Value'Class);

   function To_Json
     (Writer : Json_Writer)
      return Hera.Json.Json_Value'Class;

private

   type String_Writer is
     new Hera.Writers.Writer_Interface with
      record
         Target : Ada.Strings.Unbounded.Unbounded_String;
      end record;

   type Json_Writer is
     new Hera.Writers.Writer_Interface with
      record
         Output_Lines     : Hera.Json.Json_Array;
         Error_Lines      : Hera.Json.Json_Array;
         Current_Output   : Ada.Strings.Unbounded.Unbounded_String;
         Control          : Hera.Json.Json_Array;
         Result           : Hera.Json.Json_Object;
      end record;

end Hera.Commands.Writers;
