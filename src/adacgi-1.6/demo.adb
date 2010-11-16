with CGI, Text_IO; use CGI, Text_IO;

procedure Demo is
-- Demonstrate CGI interface.   See the examples at 
-- http://www.ncsa.uiuc.edu/SDG/Software/Mosaic/Docs/fill-out-forms/overview.html

-- To run this program directly (without an HTTP server), set the
-- environment variable REQUEST_METHOD to "GET" and the variable
-- QUERY_STRING to either "" or "name=David&topping=anchovies&callfirst=no".

begin
  Put_CGI_Header;

  if CGI.Input_Received then
    Put_HTML_Head("Form Result of Demo Ada 95 Binding to CGI");
    Put_HTML_Heading("Form Result of Demo", 1);
    Put_Line("<p>Your name is <i>" & HTML_Encode(Value("name")) & "</i>");
    Put_Line("<p>The keys and values sent were:<p>");
    Put_Variables;
  else
    Put_HTML_Head("Demonstration of Ada 95 Binding to CGI");
    Put_HTML_Heading("AdaCGI Demonstration Form", 1);
    Put_Line("<p>This form demonstrates an Ada 95 binding to CGI.<p>");

    Put_Line("<form method=""POST"">");

    Put_Line("What is your name: <input name=""name"" size=""40"">");

    Put_Line("<p>What topping would you like on your pizza?<p><ol>");
    Put_Line("<li><input type=""checkbox"" name=""topping"" " &
             "value=""pepperoni"" checked>Pepperoni.");
    Put_Line("<li><input type=""checkbox"" name=""topping"" " &
             "value=""sausage"">Sausage.");
    Put_Line("<li><input type=""checkbox"" name=""topping"" " &
             "value=""anchovies"">Anchovies.");
    Put_Line("</ol>");

    Put_Line("Would you like us to call ahead?");
    Put_Line("<dl>");
    Put_Line("<dd> <input type=""radio"" name=""callfirst"" value=""yes"" " &
             "checked> <i>Yes.</i>");
    Put_Line("<dd> <input type=""radio"" name=""callfirst"" value=""no""> " &
             "<i>No.</i>");
    Put_Line("</dl>");


    Put_Line("<p> <input type=""submit""> <input type=""reset""> ");
    Put_Line("</form>");
  end if;

  Put_HTML_Tail;
end Demo;
