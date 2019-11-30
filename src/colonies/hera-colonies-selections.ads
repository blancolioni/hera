private with Ada.Containers.Doubly_Linked_Lists;

with Ada.Iterator_Interfaces;

with Hera.Db;

package Hera.Colonies.Selections is

   type Cursor is private;

   function Has_Element (Item : Cursor) return Boolean;

   type Constant_Reference_Type
     (Element : not null access constant Colony_Handle) is private
     with Implicit_Dereference => Element;

   package Selection_Iterator_Interfaces is
     new Ada.Iterator_Interfaces
       (Cursor,
        Has_Element);

   type Selection is tagged limited private
     with Constant_Indexing => Constant_Reference,
     Default_Iterator => Iterate,
     Iterator_Element => Colony_Handle;

   function Iterate
     (Container : Selection)
      return Selection_Iterator_Interfaces.Reversible_Iterator'Class;

   function Constant_Reference
     (Container : aliased Selection;
      Position  : Cursor)
      return Constant_Reference_Type;

   function Is_Empty (Container : Selection) return Boolean;

   function Element (Item : Cursor) return Colony_Handle;

   function Select_All return Selection;

   type Colony_Key is tagged private;

   function Top_Record_Key return Colony_Key;
   function Top_Record_Key (Value : Hera.Db.Record_Type) return Colony_Key;

   function Identifier_Key return Colony_Key;
   function Identifier_Key (Value : String) return Colony_Key;

   function From return Colony_Key;

   function Select_By (Key : Colony_Key) return Selection;

private

   package Element_Lists is
     new Ada.Containers.Doubly_Linked_Lists
       (Colony_Handle, Hera.Handles.Colony."=");

   type Cursor is
      record
         Position : Element_Lists.Cursor;
      end record;

   type Selection is tagged limited
      record
         Data_Source : Element_Lists.List;
      end record;

   type Constant_Reference_Type
     (Element : not null access constant Colony_Handle) is
      record
         Container : access Selection;
      end record;

   type Colony_Key is tagged null record;

end Hera.Colonies.Selections;
