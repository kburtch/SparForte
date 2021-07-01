#include <locale.h>
#include <langinfo.h>

/*
 * Localization features
 */

const int l10n_LC_ALL = LC_ALL;            // All of the locale
const int l10n_LC_COLLATE = LC_COLLATE;    // String collation
const int l10n_LC_MESSAGES = LC_MESSAGES;  // Localizable natural-language messages
const int l10n_LC_MONETARY = LC_MONETARY;  // Formatting of monetary values
const int l10n_LC_CTYPE = LC_CTYPE;        // Character classification
const int l10n_LC_TIME = LC_TIME;           // Formatting of date and time values

/*
       LC_ADDRESS          Formatting of addresses and
                           geography-related items (*)
       LC_IDENTIFICATION   Metadata describing the locale (*)
       LC_MEASUREMENT      Settings related to measurements
                           (metric versus US customary) (*)
       LC_NAME             Formatting of salutations for persons (*)
       LC_NUMERIC          Formatting of nonmonetary numeric values
       LC_PAPER            Settings related to the standard paper size (*)
       LC_TELEPHONE        Formats to be used with telephone services (*)
*/


/**
 *  GET LOCAL
 */

char *getlocale(int lc) {
  return setlocale( lc, NULL );
}

/**
 *  LANGINFO CODESET
 *
 */

char *langinfo_codeset() {
   return nl_langinfo( CODESET );
}


/**
 *  LANGINFO D_T_FMT
 *
 */

char *langinfo_d_t_fmt() {
   return nl_langinfo( D_T_FMT );
}


/**
 *  LANGINFO D_FMT
 *
 */

char *langinfo_d_fmt() {
   return nl_langinfo( D_FMT );
}


/**
 *  LANGINFO T_FMT
 *
 */

char *langinfo_t_fmt() {
   return nl_langinfo( T_FMT );
}


/**
 *  LANGINFO DAY 1
 *
 */

char *langinfo_day_1() {
   return nl_langinfo( DAY_1 );
}


/**
 *  LANGINFO DAY 2
 *
 */

char *langinfo_day_2() {
   return nl_langinfo( DAY_2 );
}


/**
 *  LANGINFO DAY 3
 *
 */

char *langinfo_day_3() {
   return nl_langinfo( DAY_3 );
}


/**
 *  LANGINFO DAY 4
 *
 */

char *langinfo_day_4() {
   return nl_langinfo( DAY_4 );
}


/**
 *  LANGINFO DAY 5
 *
 */

char *langinfo_day_5() {
   return nl_langinfo( DAY_5 );
}


/**
 *  LANGINFO DAY 6
 *
 */

char *langinfo_day_6() {
   return nl_langinfo( DAY_6 );
}


/**
 *  LANGINFO DAY 7
 *
 */

char *langinfo_day_7() {
   return nl_langinfo( DAY_7 );
}


/**
 *  LANGINFO AB(breviated) DAY 1
 *
 */

char *langinfo_abday_1() {
   return nl_langinfo( ABDAY_1 );
}


/**
 *  LANGINFO AB(breviated) DAY 2
 *
 */

char *langinfo_abday_2() {
   return nl_langinfo( ABDAY_2 );
}


/**
 *  LANGINFO AB(breviated) DAY 3
 *
 */

char *langinfo_abday_3() {
   return nl_langinfo( ABDAY_3 );
}


/**
 *  LANGINFO AB(breviated) DAY 4
 *
 */

char *langinfo_abday_4() {
   return nl_langinfo( ABDAY_4 );
}


/**
 *  LANGINFO AB(breviated) DAY 5
 *
 */

char *langinfo_abday_5() {
   return nl_langinfo( ABDAY_5 );
}


/**
 *  LANGINFO AB(breviated) DAY 6
 *
 */

char *langinfo_abday_6() {
   return nl_langinfo( ABDAY_6 );
}


/**
 *  LANGINFO AB(breviated) DAY 7
 *
 */

char *langinfo_abday_7() {
   return nl_langinfo( ABDAY_7 );
}


/**
 *  LANGINFO MON(th) 1
 *
 */

char *langinfo_mon_1() {
   return nl_langinfo( MON_1 );
}


/**
 *  LANGINFO MON(th) 2
 *
 */

char *langinfo_mon_2() {
   return nl_langinfo( MON_2 );
}


/**
 *  LANGINFO MON(th) 3
 *
 */

char *langinfo_mon_3() {
   return nl_langinfo( MON_3 );
}


/**
 *  LANGINFO MON(th) 4
 *
 */

char *langinfo_mon_4() {
   return nl_langinfo( MON_4 );
}


/**
 *  LANGINFO MON(th) 5
 *
 */

char *langinfo_mon_5() {
   return nl_langinfo( MON_5 );
}


/**
 *  LANGINFO MON(th) 6
 *
 */

char *langinfo_mon_6() {
   return nl_langinfo( MON_6 );
}


/**
 *  LANGINFO MON(th) 7
 *
 */

char *langinfo_mon_7() {
   return nl_langinfo( MON_7 );
}


/**
 *  LANGINFO MON(th) 8
 *
 */

char *langinfo_mon_8() {
   return nl_langinfo( MON_8 );
}


/**
 *  LANGINFO MON(th) 9
 *
 */

char *langinfo_mon_9() {
   return nl_langinfo( MON_9 );
}


/**
 *  LANGINFO MON(th) 10
 *
 */

char *langinfo_mon_10() {
   return nl_langinfo( MON_10 );
}


/**
 *  LANGINFO MON(th) 11
 *
 */

char *langinfo_mon_11() {
   return nl_langinfo( MON_11 );
}


/**
 *  LANGINFO MON(th) 12
 *
 */

char *langinfo_mon_12() {
   return nl_langinfo( MON_12 );
}


/**
 *  LANGINFO AB(breviated) MON(th) 1
 *
 */

char *langinfo_abmon_1() {
   return nl_langinfo( ABMON_1 );
}


/**
 *  LANGINFO AB(breviated) MON(th) 2
 *
 */

char *langinfo_abmon_2() {
   return nl_langinfo( ABMON_2 );
}


/**
 *  LANGINFO AB(breviated) MON(th) 3
 *
 */

char *langinfo_abmon_3() {
   return nl_langinfo( ABMON_3 );
}


/**
 *  LANGINFO AB(breviated) MON(th) 4
 *
 */

char *langinfo_abmon_4() {
   return nl_langinfo( ABMON_4 );
}


/**
 *  LANGINFO AB(breviated) MON(th) 5
 *
 */

char *langinfo_abmon_5() {
   return nl_langinfo( ABMON_5 );
}


/**
 *  LANGINFO AB(breviated) MON(th) 6
 *
 */

char *langinfo_abmon_6() {
   return nl_langinfo( ABMON_6 );
}


/**
 *  LANGINFO AB(breviated) MON(th) 7
 *
 */

char *langinfo_abmon_7() {
   return nl_langinfo( ABMON_7 );
}


/**
 *  LANGINFO AB(breviated) MON(th) 8
 *
 */

char *langinfo_abmon_8() {
   return nl_langinfo( ABMON_8 );
}


/**
 *  LANGINFO AB(breviated) MON(th) 9
 *
 */

char *langinfo_abmon_9() {
   return nl_langinfo( ABMON_9 );
}


/**
 *  LANGINFO AB(breviated) MON(th) 10
 *
 */

char *langinfo_abmon_10() {
   return nl_langinfo( ABMON_10 );
}


/**
 *  LANGINFO AB(breviated) MON(th) 11
 *
 */

char *langinfo_abmon_11() {
   return nl_langinfo( ABMON_11 );
}


/**
 *  LANGINFO AB(breviated) MON(th) 12
 *
 */

char *langinfo_abmon_12() {
   return nl_langinfo( ABMON_12 );
}


/**
 *  LANGINFO DECIMAL POINT
 *
 */

char *langinfo_decimal_point() {
   return nl_langinfo( DECIMAL_POINT );
}


/**
 *  LANGINFO THOUSANDS SEP(erator)
 *
 */

char *langinfo_thousands_sep() {
   return nl_langinfo( THOUSANDS_SEP );
}


/**
 *  LANGINFO YES EXPR
 *
 */

char *langinfo_yesexpr() {
   return nl_langinfo( YESEXPR );
}


/**
 *  LANGINFO NO EXPR
 *
 */

char *langinfo_noexpr() {
   return nl_langinfo( NOEXPR );
}


/**
 *  LANGINFO CURRENCY SYMBOL
 *
 */

char *langinfo_currency_symbol() {
   return nl_langinfo( CURRENCY_SYMBOL );
}


/**
 *  LANGINFO AM STR
 *
 */

char *langinfo_amstr() {
   return nl_langinfo( AM_STR );
}


/**
 *  LANGINFO PM STR
 *
 */

char *langinfo_pmstr() {
   return nl_langinfo( PM_STR );
}


/**
 *  LANGINFO T_FMT_AMPM (12 hour time)
 *
 */

char *langinfo_t_fmt_ampm() {
   return nl_langinfo( T_FMT_AMPM );
}


/**
 *  LANGINFO POSITIVE SIGN
 *
 */

char *langinfo_positive_sign() {
   return nl_langinfo( POSITIVE_SIGN );
}


/**
 *  LANGINFO NEGATIVE SIGN
 *
 */

char *langinfo_negative_sign() {
   return nl_langinfo( NEGATIVE_SIGN );
}


/**
 *  LANGINFO INT CURR SYMBOL
 *
 */

char *langinfo_int_curr_symbol() {
   return nl_langinfo( INT_CURR_SYMBOL );
}

