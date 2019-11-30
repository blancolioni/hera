private with WL.String_Maps;

generic
   type Element_Type is private;
   with function "=" (Left, Right : Element_Type) return Boolean is <>;
package Hera.Commodities.Maps is

   type Variable_Reference (Element : access Element_Type) is private
     with Implicit_Dereference => Element;

   type Map is tagged private;

   function Contains (Container : Map;
                      Commodity : not null access constant
                        Root_Commodity_Type'Class)
                      return Boolean;

   function Element (Container : Map;
                     Commodity : not null access constant
                       Root_Commodity_Type'Class)
                     return Element_Type
     with Pre => Container.Contains (Commodity);

   function Reference (Container : in out Map;
                       Commodity : not null access constant
                         Root_Commodity_Type'Class)
                       return Variable_Reference
     with Pre => Container.Contains (Commodity);

   procedure Insert (Container : in out Map;
                     Commodity : not null access constant
                       Root_Commodity_Type'Class;
                     Element   : Element_Type);

private

   type Variable_Reference (Element : access Element_Type) is null record;

   package Commodity_Maps is
     new WL.String_Maps (Element_Type, "=");

   type Map is new Commodity_Maps.Map with null record;

   function Contains (Container : Map;
                      Commodity : not null access constant
                        Root_Commodity_Type'Class)
                      return Boolean
   is (Container.Contains (Commodity.Tag));

   function Element (Container : Map;
                     Commodity : not null access constant
                       Root_Commodity_Type'Class)
                     return Element_Type
   is (Container.Element (Commodity.Tag));

   function Reference (Container : in out Map;
                       Commodity : not null access constant
                         Root_Commodity_Type'Class)
                       return Variable_Reference
   is (Variable_Reference'
         (Element => Container.Reference (Commodity.Tag).Element));

end Hera.Commodities.Maps;
