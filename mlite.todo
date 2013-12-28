
#include "pch.hxx"  //pch
 #include <string>
#include <sstream>

#include <boost/spirit/home/support/info.hpp>
#include <boost/spirit/include/support_line_pos_iterator.hpp>

#include <string>

#include <boost/cstdint.hpp>

#include <boost/regex/pending/unicode_iterator.hpp>

#include <boost/spirit/include/qi.hpp>
#include <boost/spirit/include/phoenix_core.hpp>
#include <boost/spirit/include/phoenix_container.hpp>
#include <boost/spirit/include/phoenix_statement.hpp>
#include <boost/spirit/include/phoenix_operator.hpp>
#include <boost/spirit/include/support_utree.hpp>

namespace utf8
{

namespace qi = boost::spirit::qi;
namespace px = boost::phoenix;
namespace standard = boost::spirit::standard;

using boost::spirit::utree;
using boost::spirit::utf8_symbol_type;
using boost::spirit::utf8_string_type;
using boost::spirit::binary_string_type;

typedef boost::uint32_t uchar;

struct push_string
{
    template <typename, typename>
    struct result
    {
        typedef void type;
    };

    void operator()(std::string& s, uchar code_point) const
    {
        typedef std::back_insert_iterator<std::string> insert_iter;
        insert_iter out_iter(s);
        boost::utf8_output_iterator<insert_iter> s_iter(out_iter);
        *s_iter++ = code_point;
    }
};

struct push_escaped_string
{
    template <typename, typename>
    struct result
    {
        typedef void type;
    };

    void operator()(std::string& s, uchar c) const
    {
        switch (c) {
            case 'b':
                s += '\b';
                break;
            case 't':
                s += '\t';
                break;
            case 'n':
                s += '\n';
                break;
            case 'f':
                s += '\f';
                break;
            case 'r':
                s += '\r';
                break;
            case '"':
                s += '"';
                break;
            case '\\':
                s += '\\';
                break;
        }
    }
};

template <typename Iterator>
struct parser : qi::grammar<Iterator, std::string()>
{
    qi::rule<Iterator, void(std::string&)>
        escaped;

    qi::rule<Iterator, std::string()>
        start;

    px::function<push_string>
        push_str;

    px::function<push_escaped_string>
        push_esc;

    parser() : parser::base_type (start)
    {
        using standard::char_;
        using qi::uint_parser;
        using qi::_val;
        using qi::_r1;
        using qi::_1;

        uint_parser<uchar, 16, 4, 4> hex4;
        uint_parser<uchar, 16, 8, 8> hex8;

        escaped
          = '\\'
          > (   ('u' > hex4)                 [push_str(_r1, _1)]
            |   ('U' > hex8)                 [push_str(_r1, _1)]
            |   char_("btnfr\\\"'")          [push_esc(_r1, _1)]
            );

        start
          = '"'
          > *(escaped(_val) | (~char_('"'))  [_val += _1])
          > '"';

        escaped.name("escaped_string");
        start.name("string");
    }
};

} // utf8
namespace sexpr
{

using boost::spirit::info;

template <typename Out>
struct print_info
{
    typedef boost::spirit::utf8_string string;

    print_info(Out& out) : out(out), first(true) {}

    void element(string const& tag, string const& value, int) const
    {
        if (!first) {
            out << ' ';
            first = false;
        }

        if (value == "")
            out << tag;
        else
            out << "\"" << value << '"';
    }

    Out& out;
    mutable bool first;
};

struct expected_component : std::exception
{
    std::string msg;

    expected_component(std::string const& source, std::size_t line   , info const& w)
    {
        using boost::spirit::basic_info_walker;

        std::ostringstream oss;
        oss << "(exception \"" << source << "\" ";

        if (line == -1)
          oss << -1;
        else
          oss << line;

        oss << " '(expected_component (";

        print_info<std::ostringstream> pr(oss);
        basic_info_walker<print_info<std::ostringstream> >
        walker(pr, w.tag, 0);

        boost::apply_visitor(walker, w.value);

        oss << ")))";

        msg = oss.str();
    }

    virtual ~expected_component() throw() {}

    virtual char const* what() const throw()
    {
        return msg.c_str();
    }
};

template <typename Iterator>
struct error_handler
{
    template <typename, typename, typename, typename>
    struct result
    {
        typedef void type;
    };

    std::string source;

    error_handler(std::string const& source_ = "<string>") : source(source_) {}

    void operator()(Iterator first, Iterator last, Iterator err_pos
                  , info const& what) const
    {
        using boost::spirit::get_line;
        Iterator eol = err_pos;
        std::size_t line = get_line(err_pos);
        throw expected_component(source, line, what);
    }
};

} // sexpr
namespace boost {
namespace spirit {
namespace traits {

template<>
struct transform_attribute<utree::nil_type, unused_type, qi::domain> {
  typedef unused_type type;

  static unused_type pre (utree::nil_type&) { return unused_type(); }
  static void post (utree::nil_type&, unused_type) { }
  static void fail (utree::nil_type&) { }
};

} // traits
} // spirit
} // boost

namespace sexpr
{

namespace qi = boost::spirit::qi;
namespace px = boost::phoenix;
namespace standard = boost::spirit::standard;

using boost::spirit::utree;
using boost::spirit::utf8_symbol_type;
using boost::spirit::utf8_string_type;
using boost::spirit::binary_string_type;

struct bool_input_policies
{
    template <typename Iterator, typename Attribute>
    static bool
    parse_true(Iterator& first, Iterator const& last, Attribute& attr)
    {
        using boost::spirit::qi::detail::string_parse;
        using boost::spirit::qi::bool_policies;
        using boost::spirit::qi::unused;
        using boost::spirit::traits::assign_to;
        if (string_parse("#t", first, last, unused))
        {
            assign_to(true, attr);    // result is true
            return true;
        }
        return bool_policies<bool>::parse_true(first, last, attr);
    }

    template <typename Iterator, typename Attribute>
    static bool
    parse_false(Iterator& first, Iterator const& last, Attribute& attr)
    {
        using boost::spirit::qi::detail::string_parse;
        using boost::spirit::qi::bool_policies;
        using boost::spirit::qi::unused;
        using boost::spirit::traits::assign_to;
        if (string_parse("#f", first, last, unused))
        {
            assign_to(false, attr);   // result is false
            return true;
        }
        return bool_policies<bool>::parse_false(first, last, attr);
    }
};

struct save_line_pos
{
    template <typename, typename>
    struct result
    {
        typedef void type;
    };

    template <typename Range>
    void operator()(utree& ast, Range const& rng) const
    {
        using boost::spirit::get_line;
        std::size_t n = get_line(rng.begin());
        if (n != -1)
        {
            BOOST_ASSERT(n <= (std::numeric_limits<short>::max)());
            ast.tag(n);
        }
        else
            ast.tag(-1);
    }
};

template <typename Iterator, typename F>
struct tagger : qi::grammar<Iterator, void(utree&, char)>
{
    qi::rule<Iterator, void(utree&, char)>
        start;

    qi::rule<Iterator, void(utree&)>
        epsilon;

    px::function<F>
        f;

    tagger(F f_ = F()) : tagger::base_type(start), f(f_)
    {
        using qi::omit;
        using qi::raw;
        using qi::eps;
        using qi::lit;
        using qi::_1;
        using qi::_r1;
        using qi::_r2;

        start   = omit[raw[lit(_r2)] [f(_r1, _1)]];

        epsilon = omit[raw[eps]      [f(_r1, _1)]];
    }
};

template <typename Iterator>
struct whitespace : qi::grammar<Iterator> {
    qi::rule<Iterator>
        start;

    whitespace() : whitespace::base_type(start)
    {
        using standard::space;
        using standard::char_;
        using qi::eol;

        start = space | (';' >> *(char_ - eol) >> eol);
    }
};

} // sexpr

//[utree_sexpr_parser
namespace sexpr
{

template <typename Iterator, typename ErrorHandler = error_handler<Iterator> >
struct parser : qi::grammar<Iterator, utree(), whitespace<Iterator> >
{
    qi::rule<Iterator, utree(), whitespace<Iterator> >      start ;



    px::function<ErrorHandler> const      error;


    parser(std::string const& source_file = "<string>"):
        parser::base_type(start), error(ErrorHandler(source_file))
    {
        namespace qi = boost::spirit::qi;
        namespace ascii = boost::spirit::ascii;
        namespace phoenix = boost::phoenix;
        using boost::spirit::omit;
        using boost::spirit::no_skip;
        using boost::spirit::raw;
        using boost::spirit::double_;
        using boost::spirit::utree;
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
         using qi::unused_type;
        using qi::lexeme;
        using qi::hex;
        using qi::oct;
        using qi::no_case;
        using qi::real_parser;
        using qi::strict_real_policies;
        using qi::uint_parser;
        using qi::bool_parser;
        using qi::on_error;
        using qi::fail;
        using qi::int_;
        using qi::lit;
        using qi::_val;
        using qi::_1;
        using qi::_2;
        using qi::_3;
        using qi::_4;

  qi::rule<Iterator,  utree ,  ascii::space_type> stored_definition;


        qi::symbols< >  KEYWORDS_TABLE;
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
           CLASS=lit("class"),
           INITIAL=lit("initial");

         qi::rule<Iterator, std::string(), ascii::space_type> IDENT,name,string_comment,
                      NONDIGIT            ,// char_("_") |ascii::alpha,
           DIGIT               ,// ascii::digit,
           KEYWORDS            ,// lexeme[KEYWORDS_TABLE >> !(NONDIGIT|DIGIT)],
           UNSIGNED_INTEGER    ,// lexeme[+ascii::digit],
           UNSIGNED_NUMBER     ,// UNSIGNED_INTEGER >> -(char_(".") >> -UNSIGNED_INTEGER ) >> -((char_("e")|char_("E")) >> -(char_("+") | char_("-")) >> UNSIGNED_INTEGER),
           S_CHAR              ,// char_ - (char_("\\")|char_("\"")),
           Q_CHAR              ,// char_ - (char_("\\")|char_("'")),
           S_ESCAPE            ,// (lit("\\'") | lit("\\\"") | lit("\\?") | lit("\\\\") |  lit("\\a") | lit("\\b")  | lit("\\f") | lit("\\n")  | lit("\\r") | lit("\\t") | lit("\\v")),
           STRING              ,// char_("\"") >> *(S_CHAR | S_ESCAPE) >> char_("\""),
           Q_IDENT             ;// char_("'") >> (Q_CHAR | S_ESCAPE) >> *(Q_CHAR | S_ESCAPE) >> char_("'") ;


        KEYWORDS_TABLE ="algorithm", "and", "annotation", "assert", "block", "break", "class", "connect", "connector", "constant", "constrainedby", "der", "discrete", "each", "else", "elseif",
        "elsewhen", "encapsulated", "end", "enumeration", "equation", "expandable", "extends", "external", "false", "final", "flow", "for", "function", "import", "if", "in", "initial", "inner",
        "input", "loop", "model", "not", "or", "outer", "output", "package", "parameter", "partial", "protected", "public", "record", "redeclare", "replaceable", "return", "then", "true",
        "type", "when", "while", "within";


           NONDIGIT            %= char_("_") |ascii::alpha;
           DIGIT               %= ascii::digit;
           KEYWORDS            %= lexeme[KEYWORDS_TABLE >> !(NONDIGIT|DIGIT)];
           UNSIGNED_INTEGER    %= lexeme[+ascii::digit];
           UNSIGNED_NUMBER     %= UNSIGNED_INTEGER >> -(char_(".") >> -UNSIGNED_INTEGER ) >> -((char_("e")|char_("E")) >> -(char_("+") | char_("-")) >> UNSIGNED_INTEGER);
           S_CHAR              %= char_ - (char_("\\")|char_("\""));
           Q_CHAR              %= char_ - (char_("\\")|char_("'"));
           S_ESCAPE            %= (lit("\\'") | lit("\\\"") | lit("\\?") | lit("\\\\") |  lit("\\a") | lit("\\b")  | lit("\\f") | lit("\\n")  | lit("\\r") | lit("\\t") | lit("\\v"));
           STRING              %= lit('"') >> *(S_CHAR | S_ESCAPE) >> '"';
           Q_IDENT             %= lit('\'') >> +(Q_CHAR | S_ESCAPE)  >> '\'' ;
           name  %= -DOT >> IDENT %DOT ;
           IDENT  %= raw[(lexeme[NONDIGIT >> *(NONDIGIT | DIGIT)] | Q_IDENT) - KEYWORDS],name = IDENT >> -(char_('.') >> IDENT);

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

        stored_definition= -( lit("within") >> -name >> SEMICOLON ) >> *( -FINAL >> class_definition >> SEMICOLON ) ;


        start =  stored_definition;
        start.name("start");

        on_error<fail>(start, error(_1, _2, _3, _4));
    }
};

} // sexpr
//]

#include <boost/spirit/include/support_istream_iterator.hpp>
#include <boost/spirit/include/support_line_pos_iterator.hpp>
#include <boost/spirit/include/qi_parse.hpp>


int
main()
{
    using boost::spirit::qi::phrase_parse;

    std::cout << "/////////////////////////////////////////////////////////\n\n";
    std::cout << "sexpr parser...\n\n";
    std::cout << "/////////////////////////////////////////////////////////\n\n";
    std::cout << "Type an expression... or [q or Q] to quit\n\n";

    typedef std::string::const_iterator iterator_type;
    typedef sexpr::parser<iterator_type> parser;
    typedef sexpr::whitespace<iterator_type> space;

    parser p;
    space ws;

    std::string str;
    while (std::getline(std::cin, str))
    {
        if (str.empty() || str[0] == 'q' || str[0] == 'Q')
            break;

        std::string::const_iterator iter = str.begin();
        std::string::const_iterator end = str.end();
        bool r = phrase_parse(iter, end, p, ws);

        if (r && iter == end)
        {
            std::cout << "-------------------------\n";
            std::cout << "Parsing succeeded\n";
            std::cout << "-------------------------\n";
        }
        else
        {
            std::string rest(iter, end);
            std::cout << "-------------------------\n";
            std::cout << "Parsing failed\n";
            std::cout << "stopped at: \": " << rest << "\"\n";
            std::cout << "-------------------------\n";
        }
    }

    std::cout << "Bye... :-) \n\n";
    return 0;
}

#if 0
  namespace qi = boost::spirit::qi;
    namespace ascii = boost::spirit::ascii;
namespace phoenix = boost::phoenix;
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
      qi::rule<Iterator,  utree ,  ascii::space_type> stored_definition;

        KEYWORDS_TABLE ="algorithm", "and", "annotation", "assert", "block", "break", "class", "connect", "connector", "constant", "constrainedby", "der", "discrete", "each", "else", "elseif",
        "elsewhen", "encapsulated", "end", "enumeration", "equation", "expandable", "extends", "external", "false", "final", "flow", "for", "function", "import", "if", "in", "initial", "inner",
        "input", "loop", "model", "not", "or", "outer", "output", "package", "parameter", "partial", "protected", "public", "record", "redeclare", "replaceable", "return", "then", "true",
        "type", "when", "while", "within";


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

        stored_definition= -( lit("within") >> -name >> SEMICOLON ) >> *( -FINAL >> class_definition >> SEMICOLON ) ;

 #endif
