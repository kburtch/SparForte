#include <locale.h>
#include <langinfo.h>
#include <stdio.h>

/*
 * Localization features
 */

const int l10n_LC_ALL = LC_ALL;            // All of the locale
const int l10n_LC_COLLATE = LC_COLLATE;    // String collation
const int l10n_LC_MESSAGES = LC_MESSAGES;  // Localizable natural-language messages
const int l10n_LC_MONETARY = LC_MONETARY;  // Formatting of monetary values
const int l10n_LC_CTYPE = LC_CTYPE;        // Character classification
const int l10n_LC_TIME = LC_TIME;           // Formatting of date and time values
char *undefined = "undefined";

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

// Define missing items for macOS
// Red Hat Linux disabes these also (check USE_GNU).
#if defined(__APPLE__)
#define	DECIMAL_POINT -1
#define	THOUSANDS_SEP -1
#define	CURRENCY_SYMBOL -1
#define	POSITIVE_SIGN -1
#define	NEGATIVE_SIGN -1
#define	INT_CURR_SYMBOL -1
#define	GROUPING -1
#define	FRAC_DIGITS -1
#define	INT_FRAC_DIGITS -1
#define	P_CS_PRECEDES -1
#define	P_SEP_BY_SPACE -1
#define	P_SIGN_POSN -1
#define	N_CS_PRECEDES -1
#define	N_SEP_BY_SPACE -1
#define	N_SIGN_POSN -1
#define	MON_DECIMAL_POINT -1
#define	MON_THOUSANDS_SEP -1
#define	MON_GROUPING -1
#define	ERA_YEAR -1
#endif

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
#if defined(__APPLE__) || defined(__FreeBSD__)
   return undefined;
#elif !defined(__USE_GNU)
   return undefined;
#else
   return nl_langinfo( DECIMAL_POINT );
#endif
}


/**
 *  LANGINFO THOUSANDS SEP(erator)
 *
 */

char *langinfo_thousands_sep() {
#if defined(__APPLE__) || defined(__FreeBSD__)
   return undefined;
#elif !defined(__USE_GNU)
   return undefined;
#else
   return nl_langinfo( THOUSANDS_SEP );
#endif
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
#if defined(__APPLE__) || defined(__FreeBSD__)
   return undefined;
#elif !defined(__USE_GNU)
   return undefined;
#else
   return nl_langinfo( CURRENCY_SYMBOL );
#endif
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
#if defined(__APPLE__) || defined(__FreeBSD__)
   return undefined;
#elif !defined(__USE_GNU)
   return undefined;
#else
   return nl_langinfo( POSITIVE_SIGN );
#endif
}


/**
 *  LANGINFO NEGATIVE SIGN
 *
 */

char *langinfo_negative_sign() {
#if defined(__APPLE__) || defined(__FreeBSD__)
   return undefined;
#elif !defined(__USE_GNU)
   return undefined;
#else
   return nl_langinfo( NEGATIVE_SIGN );
#endif
}


/**
 *  LANGINFO INT CURR SYMBOL
 *
 */

char *langinfo_int_curr_symbol() {
#if defined(__APPLE__) || defined(__FreeBSD__)
   return undefined;
#elif !defined(__USE_GNU)
   return undefined;
#else
   return nl_langinfo( INT_CURR_SYMBOL );
#endif
}


/**
 *  LANGINFO GROUPING (number of digits)
 *
 */

char *langinfo_grouping() {
#if defined(__APPLE__) || defined(__FreeBSD__)
   return "\0";
#elif !defined(__USE_GNU)
   return "\0";
#else
   return nl_langinfo( GROUPING );
#endif
}

/**
 *  LANGINFO FRAC (tional) DIGITS
 *
 */

char *langinfo_frac_digits() {
#if defined(__APPLE__) || defined(__FreeBSD__)
   return undefined;
#elif !defined(__USE_GNU)
   return undefined;
#else
   return nl_langinfo( FRAC_DIGITS );
#endif
}

/**
 *  LANGINFO INT(ternational) FRAC (tional) DIGITS
 *
 */

char *langinfo_int_frac_digits() {
#if defined(__APPLE__) || defined(__FreeBSD__)
   return undefined;
#elif !defined(__USE_GNU)
   return undefined;
#else
   return nl_langinfo( INT_FRAC_DIGITS );
#endif
}

/**
 *  LANGINFO P(ositive) CS PRECEDES
 *
 */

char *langinfo_p_cs_precedes() {
#if defined(__APPLE__) || defined(__FreeBSD__)
   return undefined;
#elif !defined(__USE_GNU)
   return undefined;
#else
   return nl_langinfo( P_CS_PRECEDES );
#endif
}

/**
 *  LANGINFO P(ositive) SEP BY PRECEDES
 *
 */

char *langinfo_p_sep_by_space() {
#if defined(__APPLE__) || defined(__FreeBSD__)
   return undefined;
#elif !defined(__USE_GNU)
   return undefined;
#else
   return nl_langinfo( P_SEP_BY_SPACE );
#endif
}

/**
 *  LANGINFO P(ositive) SIGN POSN (position)
 *
 */

char *langinfo_p_sign_posn() {
#if defined(__APPLE__) || defined(__FreeBSD__)
   return undefined;
#elif !defined(__USE_GNU)
   return undefined;
#else
   return nl_langinfo( P_SIGN_POSN );
#endif
}

/**
 *  LANGINFO N(egative) CS PRECEDES
 *
 */

char *langinfo_n_cs_precedes() {
#if defined(__APPLE__) || defined(__FreeBSD__)
   return undefined;
#elif !defined(__USE_GNU)
   return undefined;
#else
   return nl_langinfo( N_CS_PRECEDES );
#endif
}

/**
 *  LANGINFO N(egative) SEP BY PRECEDES
 *
 */

char *langinfo_n_sep_by_space() {
#if defined(__APPLE__) || defined(__FreeBSD__)
   return undefined;
#elif !defined(__USE_GNU)
   return undefined;
#else
   return nl_langinfo( N_SEP_BY_SPACE );
#endif
}

/**
 *  LANGINFO N(egative) SIGN POSN (position)
 *
 */

char *langinfo_n_sign_posn() {
#if defined(__APPLE__) || defined(__FreeBSD__)
   return undefined;
#elif !defined(__USE_GNU)
   return undefined;
#else
   return nl_langinfo( N_SIGN_POSN );
#endif
}

/**
 *  LANGINFO MON(etary) DECIMAL POINT
 *
 */

char *langinfo_mon_decimal_point() {
#if defined(__APPLE__) || defined(__FreeBSD__)
   return undefined;
#elif !defined(__USE_GNU)
   return undefined;
#else
   return nl_langinfo( MON_DECIMAL_POINT );
#endif
}


/**
 *  LANGINFO MON(etary) THOUSANDS SEP(erator)
 *
 */

char *langinfo_mon_thousands_sep() {
#if defined(__APPLE__) || defined(__FreeBSD__)
   return undefined;
#elif !defined(__USE_GNU)
   return undefined;
#else
   return nl_langinfo( MON_THOUSANDS_SEP );
#endif
}

/**
 *  LANGINFO MON(etary) GROUPING
 *
 */

char *langinfo_mon_grouping() {
#if defined(__APPLE__) || defined(__FreeBSD__)
   return "\0";
#elif !defined(__USE_GNU)
   return "\0";
#else
   return nl_langinfo( MON_GROUPING );
#endif
}

/**
 *  LANGINFO (Alternate) ERA
 *
 */

char *langinfo_era() {
   return nl_langinfo( ERA );
}


/**
 *  LANGINFO (Alternate) ERA YEAR
 *
 */

char *langinfo_era_year() {
#if defined(__APPLE__) || defined(__FreeBSD__)
   return undefined;
#elif !defined(__USE_GNU)
   return undefined;
#else
   return nl_langinfo( ERA_YEAR );
#endif
}


/**
 *  LANGINFO (Alternate) ERA D(ate) T(ime) FMT (Format)
 *
 */

char *langinfo_era_d_t_fmt() {
   return nl_langinfo( ERA_D_T_FMT );
}

/**
 *  LANGINFO (Alternate) ERA D(ate) FMT (Format)
 *
 */

char *langinfo_era_d_fmt() {
   return nl_langinfo( ERA_D_FMT );
}

/**
 *  LANGINFO (Alternate) ERA T(ime) FMT (Format)
 *
 */

char *langinfo_era_t_fmt() {
   return nl_langinfo( ERA_T_FMT );
}

