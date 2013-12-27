
/*=============================================================================
Copyright (c) 2001-2010 Joel de Guzman

Distributed under the Boost Software License, Version 1.0. (See accompanying
file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
=============================================================================*/
///////////////////////////////////////////////////////////////////////////////
//
//  A mini XML-like parser
//
//  [ JDG March 25, 2007 ]   spirit2
//
///////////////////////////////////////////////////////////////////////////////

#include "pch.hxx"  //pch

namespace client
{
namespace fusion = boost::fusion;
namespace phoenix = boost::phoenix;
namespace qi = boost::spirit::qi;
namespace ascii = boost::spirit::ascii;
#if 0
///////////////////////////////////////////////////////////////////////////
//  Our mini XML tree representation
///////////////////////////////////////////////////////////////////////////
struct mini_xml;

typedef
boost::variant<
boost::recursive_wrapper<mini_xml>
, std::string
>
mini_xml_node;

struct mini_xml
{
    std::string name;                           // tag name
    std::vector<mini_xml_node> children;        // children
};
#endif
}
#if 0
// We need to tell fusion about our mini_xml struct
// to make it a first-class fusion citizen
BOOST_FUSION_ADAPT_STRUCT(
    client::mini_xml,
    (std::string, name)
    (std::vector<client::mini_xml_node>, children)
)
#endif

namespace client
{
#if 0
///////////////////////////////////////////////////////////////////////////
//  Print out the mini xml tree
///////////////////////////////////////////////////////////////////////////
int const tabsize = 4;

void tab(int indent)
{
    for (int i = 0; i < indent; ++i)
        std::cout << ' ';
}

struct mini_xml_printer
{
    mini_xml_printer(int indent = 0)
        : indent(indent)
    {
    }

    void operator()(mini_xml const& xml) const;

    int indent;
};

struct mini_xml_node_printer : boost::static_visitor<>
{
    mini_xml_node_printer(int indent = 0)
        : indent(indent)
    {
    }

    void operator()(mini_xml const& xml) const
    {
        mini_xml_printer(indent + tabsize)(xml);
    }

    void operator()(std::string const& text) const
    {
        tab(indent + tabsize);
        std::cout << "text: \"" << text << '"' << std::endl;
    }

    int indent;
};

void mini_xml_printer::operator()(mini_xml const& xml) const
{
    tab(indent);
    std::cout << "tag: " << xml.name << std::endl;
    tab(indent);
    std::cout << '{' << std::endl;

    BOOST_FOREACH(mini_xml_node const& node, xml.children)
    {
        boost::apply_visitor(mini_xml_node_printer(indent), node);
    }

    tab(indent);
    std::cout << '}' << std::endl;
}

///////////////////////////////////////////////////////////////////////////
//  Our mini XML grammar definition
///////////////////////////////////////////////////////////////////////////
//[tutorial_xml3_grammar
template <typename Iterator>
struct mlite_grammar
: qi::grammar<Iterator, mini_xml(), qi::locals<std::string>, ascii::space_type>
{
    mlite_grammar()
        : mlite_grammar::base_type(xml, "xml")
{
    using qi::lit;
    using qi::lexeme;
    using qi::on_error;
    using qi::fail;
    using ascii::char_;
    using ascii::string;
    using namespace qi::labels;

    using phoenix::construct;
    using phoenix::val;

    text %= lexeme[+(char_ - '<')];
    node %= xml | text;

    start_tag %=
        '<'
        >> !lit('/')
        > lexeme[+(char_ - '>')]
        > '>'
        ;

    end_tag =
        "</"
        > string(_r1)
        > '>'
        ;

    xml %=
        start_tag[_a = _1]
        > *node
        > end_tag(_a)
        ;

    xml.name("xml");
    node.name("node");
    text.name("text");
    start_tag.name("start_tag");
    end_tag.name("end_tag");

    on_error<fail>
    (
        xml
        , std::cout
        << val("Error! Expecting ")
        << _4                               // what failed?
        << val(" here: \"")
        << construct<std::string>(_3, _2)   // iterators to error-pos, end
        << val("\"")
        << std::endl
    );
}

qi::rule<Iterator, mini_xml(), qi::locals<std::string>, ascii::space_type> xml;
qi::rule<Iterator, mini_xml_node(), ascii::space_type> node;
qi::rule<Iterator, std::string(), ascii::space_type> text;
qi::rule<Iterator, std::string(), ascii::space_type> start_tag;
qi::rule<Iterator, void(std::string), ascii::space_type> end_tag;
};
//]
#endif // 0

template <typename Iterator>struct mlite_grammar: qi::grammar<Iterator, ascii::space_type>
{
    mlite_grammar(): mlite_grammar::base_type()
    {
        using boost::spirit::omit;
        using boost::spirit::no_skip;
        using boost::spirit::raw;
        using boost::spirit::double_;
        using boost::spirit::utree;
        using qi::lit;
        using qi::lexeme;
        using qi::on_error;
        using qi::fail;
        using ascii::char_;
        using ascii::string;
        using ascii::alpha;
        using ascii::alnum;
        using namespace qi::labels;
        using phoenix::construct;
        using phoenix::val;
        using qi::symbols;
        qi::symbols< >  KEYWORDS_TABLE;
        KEYWORDS_TABLE ="algorithm", "and", "annotation", "assert", "block", "break", "class", "connect", "connector", "constant", "constrainedby", "der", "discrete", "each", "else", "elseif",
        "elsewhen", "encapsulated", "end", "enumeration", "equation", "expandable", "extends", "external", "false", "final", "flow", "for", "function", "import", "if", "in", "initial", "inner",
        "input", "loop", "model", "not", "or", "outer", "output", "package", "parameter", "partial", "protected", "public", "record", "redeclare", "replaceable", "return", "then", "true",
        "type", "when", "while", "within";

        qi::rule<Iterator,   ascii::space_type> long_class_specifier , short_class_specifier , der_class_specifier , enumeration_literal , external_function_call , import_clause , declaration ,
           for_index , component_reference , named_argument , primary , language_specification , class_definition , class_prefixes , class_specifier , base_prefix , enum_list , composition ,
           element , extends_clause , constraining_clause , component_clause , type_prefix , type_specifier , component_list , component_declaration , condition_attribute , modification ,
           class_modification , argument_list , argument , element_modification_or_replaceable , element_modification , element_redeclaration , element_replaceable , component_clause1 ,
           component_declaration1 , short_class_definition , equation_section , algorithm_section , element_list ,  equation , statement , if_equation , if_statement ,
           for_equation , for_statement , for_indices , while_statement , when_equation , when_statement , connect_clause , expression , simple_expression , logical_expression , logical_term ,
           logical_factor , relation , rel_op , arithmetic_expression , add_op , term , mul_op , factor , bool_op , function_call_args , function_arguments , named_arguments , function_argument ,
           output_expression_list , expression_list , array_subscripts , subscript , comment , annotation  ,
           DOT=lit('.'), COLON=lit(':'), DER=lit("der"), ELSE=lit("else"), IF=lit("if"), WHEN=lit("when"), WHILE=lit("while"), THEN=lit("then"), FOR=lit("for"), END=lit("end"), OR=lit("or"),
           AND=lit("and"), FINAL=lit("final"), COMMA=lit(','), EQ=lit('='), SEMICOLON=lit(','), LOOP=lit("loop"), ELSEWHEN=lit("elsewhen"), ELSEIF=lit("elseif"), FUNCTION=lit("function"),
           CLASS=lit("class"), INITIAL=lit("initial"),
           NONDIGIT            = char_("_") | char_("a-z") | char_("A-Z"),
           DIGIT               = char_("0-9"),
           KEYWORDS            = lexeme[KEYWORDS_TABLE >> !(NONDIGIT|DIGIT)],
           UNSIGNED_INTEGER    = lexeme[DIGIT >> *DIGIT],
           UNSIGNED_NUMBER     = UNSIGNED_INTEGER >> -(char_(".") >> -UNSIGNED_INTEGER ) >> -((char_("e")|char_("E")) >> -(char_("+") | char_("-")) >> UNSIGNED_INTEGER),
           S_CHAR              = char_ - (char_("\\")|char_("\"")),
           Q_CHAR              = char_ - (char_("\\")|char_("'")),
           S_ESCAPE            = (lit("\\'") | lit("\\\"") | lit("\\?") | lit("\\\\") |  lit("\\a") | lit("\\b")  | lit("\\f") | lit("\\n")  | lit("\\r") | lit("\\t") | lit("\\v")),
           STRING              = char_("\"") >> *(S_CHAR | S_ESCAPE) >> char_("\""),
           Q_IDENT             = char_("'") >> (Q_CHAR | S_ESCAPE) >> *(Q_CHAR | S_ESCAPE) >> char_("'") ;
        qi::rule<Iterator, std::string(), ascii::space_type> IDENT,name,string_comment;

        IDENT %= raw[(lexeme[NONDIGIT >> *(NONDIGIT | DIGIT)] | Q_IDENT) - KEYWORDS],name = IDENT >> -(char_('.') >> IDENT);

        class_definition = -lit("encapsulated" ) >> class_prefixes >> class_specifier ;
        class_specifier = long_class_specifier | short_class_specifier | der_class_specifier ;
        class_prefixes =   -lit("partial")  >> ( CLASS | "model" | "record" | "block"| -lit("expandable") >> "connector" | "type" | "package" | FUNCTION | "operator" >> -( FUNCTION | "record" ) ) ;
        long_class_specifier = IDENT >> string_comment >> composition >> END >> IDENT | "extends">> IDENT >> -class_modification >> string_comment >> composition >> END >> IDENT ;
        short_class_specifier = IDENT >> EQ >> base_prefix >> name >> -array_subscripts >> -class_modification >> comment | IDENT >> EQ >> "enumeration" >> '(' >> ( -enum_list | ':' ) >> ')' >> comment ;
        der_class_specifier = IDENT >> EQ >> DER >> '(' >> name >> +( COMMA >> IDENT ) >> ')' >> comment ;
        enumeration_literal = IDENT >> comment ;
        external_function_call = -( component_reference >> EQ ) >> IDENT >> '(' >> -expression_list >> ')' ;
        import_clause = lit("import" ) >> ( IDENT >> EQ >> name | name >> -( DOT >> '*' | ".*" ) ) >> comment ;
        declaration = IDENT >> -array_subscripts >> -modification ;
        for_index = IDENT >> -( lit("in") >> expression ) ;
        name = -DOT >> IDENT %DOT     ;
        component_reference = -DOT >> IDENT >> -array_subscripts >> *( DOT >> IDENT >> -array_subscripts ) ;
        named_argument = IDENT >> EQ >> function_argument ;
        primary = UNSIGNED_NUMBER | STRING | bool_op | component_reference | ( name | DER| INITIAL) >> function_call_args | '(' >> output_expression_list >> ')' | '[' >> expression_list%';'  >> ']' | '{' >> function_arguments >> '}' | END;
        string_comment = -( STRING%'+'  ) ;
        language_specification = STRING ;
        base_prefix = type_prefix ;
        enum_list = enumeration_literal % COMMA  ;
        composition = element_list >> *( lit("public") >> element_list | "protected" >> element_list | equation_section | algorithm_section ) >> -( lit("external") >> -language_specification >> -external_function_call >> -annotation >> SEMICOLON ) >> -( annotation >> SEMICOLON ) ;
        element = import_clause | extends_clause |  -(lit("redeclare") ) >> -FINAL >> -lit("inner") >> -( lit("outer") ) >> ( class_definition | component_clause | lit("replaceable" ) >> ( class_definition | component_clause ) >> -( constraining_clause >> comment ) ) ;
        extends_clause = lit("extends") >> name >> -class_modification >> -annotation ;
        constraining_clause = lit("constrainedby") >> name >> -class_modification ;
        component_clause = type_prefix >> type_specifier >> -array_subscripts >> component_list ;
        type_prefix = -( lit("flow") | "stream") >> -( lit("discrete") | "parameter" | "constant") >> -( lit("input") | "output") ;
        type_specifier = name ;
        component_list = component_declaration %COMMA  ;
        component_declaration = declaration >> -condition_attribute >> comment ;
        condition_attribute = IF >> expression ;
        modification = class_modification >> -( (EQ |":=" ) >> expression );
        class_modification = lit('(') >> -argument_list >> ')' ;
        argument_list = argument %COMMA  ;
        argument = element_modification_or_replaceable | element_redeclaration ;
        element_modification_or_replaceable = -lit("each") >> -FINAL  >> ( element_modification | element_replaceable ) ;
        element_modification = name >> -modification >> string_comment ;
        element_redeclaration = lit("redeclare") >> -lit("each") >> -FINAL  >> ( short_class_definition | component_clause1 | element_replaceable ) ;
        element_replaceable = lit("replaceable" ) >> ( short_class_definition | component_clause1 ) >> -constraining_clause ;
        component_clause1 = type_prefix >> type_specifier >> component_declaration1 ;
        component_declaration1 = declaration >> comment ;
        short_class_definition = class_prefixes >> short_class_specifier ;

        equation_section = -INITIAL >> "equation" >> *( equation >> SEMICOLON ) ;
        algorithm_section = -INITIAL >> "algorithm" >> *( statement >> SEMICOLON ) ;
        element_list = *( element >> SEMICOLON ) ;
        equation = ( simple_expression >> EQ >> expression | if_equation | for_equation | connect_clause | when_equation | name >> function_call_args ) >> comment ;
        statement = ( component_reference >> (lit( ":=" ) >> expression | function_call_args ) | '(' >> output_expression_list >> ')' >> ":=" >> component_reference >> function_call_args | "break" | "return"| if_statement | for_statement | while_statement | when_statement ) >> comment ;
        if_equation = IF >> expression >> THEN >> *( equation >> SEMICOLON ) >> *( ELSEIF >> expression >> THEN >> *( equation >> SEMICOLON ) ) >> -( ELSE >> *( equation >> SEMICOLON ) ) >> END >> IF ;
        if_statement = IF >> expression >> THEN >> *( statement >> SEMICOLON ) >> *( ELSEIF >> expression >> THEN >> *( statement >> SEMICOLON ) ) >> -( ELSE >> *( statement >> SEMICOLON ) ) >> END >> IF ;
        for_equation = FOR >> for_indices >> LOOP >> *( equation >> SEMICOLON ) >> END >> FOR ;
        for_statement = FOR >> for_indices >> LOOP >> *( statement >> SEMICOLON ) >> END >> FOR ;
        for_indices = for_index %COMMA   ;
        while_statement = WHILE >> expression >> LOOP >> *( statement >> SEMICOLON ) >> END >> WHILE ;
        when_equation = WHEN >> expression >> THEN >> *( equation >> SEMICOLON ) >> *( ELSEWHEN >> expression >> THEN >> *( equation >> SEMICOLON ) ) >> END >> WHEN ;
        when_statement = WHEN >> expression >> THEN >> *( statement >> SEMICOLON ) >> *( ELSEWHEN >> expression >> THEN >> *( statement >> SEMICOLON ) ) >> END >> WHEN ;
        connect_clause = lit("connect") >> '(' >> component_reference >> COMMA >> component_reference >> ')' ;
        expression = simple_expression | IF >> expression >> THEN >> expression >> *( ELSEIF >> expression >> THEN >> expression ) >> ELSE >> expression ;
        simple_expression = logical_expression >> -( COLON >> logical_expression >> -( COLON >> logical_expression ) ) ;
        logical_expression = logical_term % OR   ;
        logical_term = logical_factor% AND   ;
        logical_factor = -lit("not") >> relation ;
        relation = arithmetic_expression >> -( rel_op >> arithmetic_expression ) ;
        rel_op = lit('<') | "<=" | '>' | ">=" | "==" | "<>" ;
        arithmetic_expression = -add_op >> term % add_op  ;
        add_op = lit('+') | '-' | ".+" | ".-" ;
        term = factor % mul_op ;
        mul_op = lit('*') | '/' | ".*" | "./" ;
        factor = primary >> -( ( lit('^') | ".^" ) >> primary ) ;
        bool_op = lit("false") | "true" ;
        function_call_args = lit('(') >> -function_arguments >> ')' ;
        function_arguments = function_argument >> -( COMMA >> function_arguments | FOR >> for_indices ) | named_arguments ;
        named_arguments = named_argument % COMMA ;
        function_argument = FUNCTION >> name >> '(' >> -named_arguments >> ')' | expression ;
        output_expression_list = -expression >> *( COMMA >> -expression ) ;
        expression_list = expression %COMMA;
        array_subscripts = lit('[') >> subscript % COMMA >> ']' ;
        subscript = COLON | expression ;
        comment = string_comment >> -annotation ;
        annotation = lit("annotation") >> class_modification;


        qi::rule<Iterator,  utree , ascii::space_type>    stored_definition;
        stored_definition= -( lit("within") >> -name >> SEMICOLON ) >> *( -FINAL >> class_definition >> SEMICOLON ) ;

    }
};
};
///////////////////////////////////////////////////////////////////////////////
//  Main program
///////////////////////////////////////////////////////////////////////////////
int main(int argc, char **argv)
{

    char const* filename;
    if (argc > 1)
    {
        filename = argv[1];
    }
    else
    {
        std::cerr << "Error: No input file provided." << std::endl;
        return 1;
    }

    std::ifstream in(filename, std::ios_base::in);

    if (!in)
    {
        std::cerr << "Error: Could not open input file: "
                  << filename << std::endl;
        return 1;
    }

    std::string storage; // We will read the contents here.
    in.unsetf(std::ios::skipws); // No white space skipping!
    std::copy(
        std::istream_iterator<char>(in),
        std::istream_iterator<char>(),
        std::back_inserter(storage));

    typedef client::mlite_grammar<std::string::const_iterator> mlite_grammar;
    mlite_grammar xml; // Our grammar


    using boost::spirit::ascii::space;
    std::string::const_iterator iter = storage.begin();
    std::string::const_iterator end = storage.end();
    bool r = phrase_parse(iter, end, xml, space );

    if (r && iter == end)
    {
        std::cout << "-------------------------\n";
        std::cout << "Parsing succeeded\n";
        std::cout << "-------------------------\n";
        return 0;
    }
    else
    {
        std::cout << "-------------------------\n";
        std::cout << "Parsing failed\n";
        std::cout << "-------------------------\n";
        return 1;
    }

}
