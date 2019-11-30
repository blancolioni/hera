private with Ada.Containers.Doubly_Linked_Lists;

with Ada.Iterator_Interfaces;

with Hera.Db;

with Hera.Handles.Has_Stock;

package Hera.Handles.Stock_Item.Selections is

   type Cursor is private;

   function Has_Element (Item : Cursor) return Boolean;

   type Constant_Reference_Type
     (Element : not null access constant Stock_Item_Handle) is private
     with Implicit_Dereference => Element;

   package Selection_Iterator_Interfaces is
     new Ada.Iterator_Interfaces
       (Cursor,
        Has_Element);

   type Selection is tagged limited private
     with Constant_Indexing => Constant_Reference,
     Default_Iterator => Iterate,
     Iterator_Element => Stock_Item_Handle;

   function Iterate
     (Container : Selection)
      return Selection_Iterator_Interfaces.Reversible_Iterator'Class;

   function Constant_Reference
     (Container : aliased Selection;
      Position  : Cursor)
      return Constant_Reference_Type;

   function Is_Empty (Container : Selection) return Boolean;

   function Element (Item : Cursor) return Stock_Item_Handle;

   function Select_All return Selection;

   type Stock_Item_Key is tagged private;

   function Top_Record return Stock_Item_Key;
   function Top_Record (Value : Hera.Db.Record_Type) return Stock_Item_Key;

   function Has_Stock return Stock_Item_Key;
   function Has_Stock (Value : Hera.Handles.Has_Stock.Has_Stock_Class)
                       return Stock_Item_Key;

   function From return Stock_Item_Key;

   function Select_By (Key : Stock_Item_Key'Class) return Selection;
   function Get_By (Key : Stock_Item_Key'Class) return Stock_Item_Handle;

private

   package Element_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Stock_Item_Handle);

   type Cursor is
      record
         Position : Element_Lists.Cursor;
      end record;

   type Selection is tagged limited
      record
         Data_Source : Element_Lists.List;
      end record;

   type Constant_Reference_Type
     (Element : not null access constant Stock_Item_Handle) is
      record
         Container : access Selection;
      end record;

   type Stock_Item_Key is tagged null record;

end Hera.Handles.Stock_Item.Selections;
