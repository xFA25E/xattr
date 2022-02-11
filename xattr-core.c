#include <assert.h>
#include <errno.h>
#include <limits.h>
#include <signal.h>
#include <stdalign.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/xattr.h>

#include <verify.h>             /* from Gnulib */

#include <emacs-module.h>

/* The next three static assertions check that pointers are 64 bits and properly
 * aligned.  This avoids a bug that can cause non_local_exit_get to exit
 * nonlocally by failing compilation if the bug is possible. */
verify (CHAR_BIT == 8);
verify (sizeof (emacs_value) == 8);
verify (alignof (emacs_value) == 8);

/* Type alias for an emacs function */
typedef emacs_value (*emacs_subr) (emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data);

int plugin_is_GPL_compatible;

static emacs_value
intern(emacs_env *env, const char *symbol_name)
{
    return env->intern(env, symbol_name);
}

static emacs_value
nil(emacs_env *env)
{
    return intern(env, "nil");
}

static emacs_value
funcall(emacs_env *env, const char *symbol_name, ptrdiff_t nargs, emacs_value args[])
{
    return env->funcall(env, intern(env, symbol_name), nargs, args);
}

static emacs_value
cons(emacs_env *env, emacs_value car, emacs_value cdr)
{
    emacs_value args[2] = {car, cdr};
    return funcall(env, "cons", 2, args);
}

/* Signal emacs xattr-malloc in some function */
static void
signal_malloc(emacs_env *env, const char *place)
{
    emacs_value symbol = intern(env, "xattr-malloc");
    emacs_value data = cons(env, intern(env, place), nil(env));
    env->non_local_exit_signal(env, symbol, data);
}

/* Return a symbol representation of C errno or errno integer. */
static emacs_value
errnoname(emacs_env *env, int errno_)
{
    switch (errno_){
    case EACCES: return intern(env, "EACCES");
    case EINVAL: return intern(env, "EINVAL");
    case ELOOP: return intern(env, "ELOOP");
    case ENAMETOOLONG: return intern(env, "ENAMETOOLONG");
    case ENOENT: return intern(env, "ENOENT");
    case EDQUOT: return intern(env, "EDQUOT");
    case ENOSPC: return intern(env, "ENOSPC");
    case EPERM: return intern(env, "EPERM");
    case ERANGE: return intern(env, "ERANGE");
    case ENOTSUP: return intern(env, "ENOTSUP");
    case ENODATA: return intern(env, "ENODATA");
    case E2BIG: return intern(env, "E2BIG");
    default: return env->make_integer(env, errno_);
    }
}

/* Signal emacs error with errno strerror */
static void
signal_errno(emacs_env *env)
{
    const char *msg = strerror(errno);
    emacs_value symbol = intern(env, "xattr-errno");
    emacs_value error = errnoname(env, errno);
    emacs_value message = env->make_string(env, msg, strlen(msg));
    emacs_value data = cons(env, error, cons(env, message, nil(env)));
    env->non_local_exit_signal(env, symbol, data);
}

/* Clone Emacs string to C string.  Return pointer to a null-terminated string.
 * On error, signal and return NULL. */
static char *
clone_string(emacs_env *env, emacs_value string)
{
    char *result = NULL; ptrdiff_t length;

    if (env->copy_string_contents(env, string, NULL, &length)) {
        result = malloc(length);
        if (result == NULL) {
            signal_malloc(env, "clone_string");
        } else if (!env->copy_string_contents(env, string, result, &length)) {
            free(result);
            result = NULL;
        }

    }

    return result;
}

/* Free nargs number of sargs */
static void
free_sargs(ptrdiff_t nargs, char **sargs)
{
    for (ptrdiff_t i = 0; i < nargs; i++) {
        free(sargs[i]);
    }
}

/* Extract sargs from emacs args.  Every exported module function accepts string
 * arguments.  In each one of this function a mechanism is needed to convert
 * Emacs strings to C null-terminated strings.  Every string in args is
 * converted to C string and put into sargs */
static bool
extract_sargs(emacs_env *env, ptrdiff_t nargs, emacs_value *args, char **sargs)
{
    for (ptrdiff_t i = 0; i < nargs; i++) {
        char *sarg = clone_string(env, args[i]);
        if (sarg == NULL) {
            free_sargs(i, sargs);
            return false;
        }
        sargs[i] = sarg;
    }
    return true;
}

/* Emacs wrapper around setxattr.  Must accept 3 strings as its arguments: FILE,
 * NAME and VALUE.  Extended attribute NAME is written to FILE with some VALUE.
 * VALUE can be empty string. */
static emacs_value
xattr_set(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data)
{
    assert (nargs == 3);

    char *sargs[3]; ptrdiff_t value_length; emacs_value result = args[2];

    if (extract_sargs(env, 3, args, sargs)) {
        env->copy_string_contents(env, args[2], NULL, &value_length);
        /* We need value length without terminating null byte */
        if (setxattr(sargs[0], sargs[1], sargs[2], value_length - 1, 0) == -1) {
            signal_errno(env);
        }

        free_sargs(3, sargs);
    }

    return result;
}

/* Emacs wrapper around getxattr.  Must accept 2 strings as its arguments: FILE,
 * NAME.  Return the value of xattr with NAME of FILE. */
static emacs_value
xattr_get(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data)
{
    assert (nargs == 2);

    char *sargs[2], *value; ssize_t value_length; emacs_value result = nil(env);

    if (extract_sargs(env, 2, args, sargs)) {
        value_length = getxattr(sargs[0], sargs[1], NULL, 0);
        if (value_length == -1) {
            signal_errno(env);
        } else {
            value = malloc(value_length + 1); /* We need to hold null byte too */
            if (value == NULL) {
                signal_malloc(env, "xattr_get");
            } else {
                value_length = getxattr(sargs[0], sargs[1], value, value_length);
                if (value_length == -1) {
                    signal_errno(env);
                } else {
                    value[value_length] = 0;
                    result = env->make_string(env, value, value_length);
                }

                free(value);
            }
        }

        free_sargs(2, sargs);
    }

    return result;
}

/* Emacs wrapper around removexattr.  Must accept 2 strings as its arguments:
 * FILE, NAME.  Remove xattr with NAME of FILE. */
static emacs_value
xattr_remove(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data)
{
    assert (nargs == 2);

    char *sargs[2]; emacs_value result = nil(env);

    if (extract_sargs(env, 2, args, sargs)) {
        if (removexattr(sargs[0], sargs[1]) == -1) {
            signal_errno(env);
        }

        free_sargs(2, sargs);
    }

    return result;
}

/* Emacs wrapper around listxattr.  Must accept 1 string as its argument: FILE.
 * Return a list of xattr names. */
static emacs_value
xattr_list(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data)
{
    assert (nargs == 1);

    char *sargs[1], *keys_buffer, *key;
    ssize_t keys_buffer_length, key_length;
    emacs_value result = nil(env);

    if (extract_sargs(env, 1, args, sargs)) {
        keys_buffer_length = listxattr(sargs[0], NULL, 0);
        if (keys_buffer_length == -1) {
            signal_errno(env);
        } else if (keys_buffer_length > 0) {
            keys_buffer = malloc(keys_buffer_length);
            if (keys_buffer == NULL) {
                signal_malloc(env, "xattr_list");
            } else {
                keys_buffer_length = listxattr(sargs[0], keys_buffer, keys_buffer_length);
                if (keys_buffer_length == -1) {
                    signal_errno(env);
                } else if (keys_buffer_length > 0) {
                    key = keys_buffer;
                    while (keys_buffer_length > 0) {
                        key_length = strlen(key);
                        result = cons(env, env->make_string(env, key, key_length), result);
                        key_length += 1;
                        keys_buffer_length -= key_length;
                        key += key_length;
                    }

                    result = funcall(env, "nreverse", 1, &result);
                }


                free(keys_buffer);
            }
        }

        free_sargs(1, sargs);
    }

    return result;
}

/* A convenient function to define emacs functions. */
static void
define_function(emacs_env *env,
                emacs_subr function,
                const char *symbol_name,
                ptrdiff_t arity,
                const char *documentation)
{
    emacs_value func = env->make_function(env, arity, arity, function, documentation, NULL);
    emacs_value symbol = intern(env, symbol_name);
    emacs_value args[] = {symbol, func};
    funcall(env, "defalias", 2, args);
}

static void
initialize_module(emacs_env *env)
{
    define_function(env, xattr_set, "xattr-core-set", 3, "Set FILE's xattr NAME to VALUE.\n\n(fn FILE NAME VALUE)");
    define_function(env, xattr_get, "xattr-core-get", 2, "Get FILE's xattr NAME.\n\n(fn FILE NAME)");
    define_function(env, xattr_remove, "xattr-core-remove", 2, "Remove FILE's xattr NAME.\n\n(fn FILE NAME)");
    define_function(env, xattr_list, "xattr-core-list", 1, "List FILE's xattrs.\n\n(fn FILE)");

    emacs_value feature = intern(env, "xattr-core");
    funcall(env, "provide", 1, &feature);
}

extern int
emacs_module_init(struct emacs_runtime *ert)
{
    assert (ert->size > 0);
    if ((size_t) ert->size < sizeof *ert) {
        return 1;
    }

    emacs_env *env = ert->get_environment(ert);
    assert (env->size > 0);
    if ((size_t) env->size < sizeof *env){
        return 2;
    }

    if (signal (SIGSEGV, SIG_DFL) == SIG_ERR) {
        return 3;
    }

    initialize_module(env);

    return 0;
}
