import glob
import os
import regex as re

re_typ = re.compile(r'\w+ *:: *')
re_brackets = re.compile(r'\(([^()]|(?R))*\)')
re_init_val1 = re.compile(r'(?<=\w)=.*?,')
re_init_val2 = re.compile(r'=.*$')
re_sep = re.compile(r', *')
re_word = re.compile(r'(\w+)')

output_dir = '.'
if not os.path.exists(output_dir):
    os.makedirs(output_dir)

# Obtain a list of context variables from common.f90
context = []
with open('common.f90', 'r') as f:
    line = ''
    while 'type, public :: Common' not in line:
        line = f.readline()

    while True:
        line = f.readline().strip(', &\n')
        if 'end type Common' in line:
            break

        if len(line):
            line = re.sub(re_brackets, '', line)
            line = re.sub(re_typ, '', line)
            line = re.sub(' ', '', line)
            line = re.sub(re_init_val1, r',', line)
            line = re.sub(re_init_val2, '', line)
            context += re.split(re_sep, line.lower())

# Get a list of all source files except for common.f90
files = [file for file in glob.glob('*.f90') if file != 'common.f90']

changed_subroutines = []

re_use_common = re.compile(r'( *)use common *')
re_subroutine_yargs_sub = re.compile(r'( *subroutine *\w+\()')
re_subroutine_nargs_sub = re.compile(r'( *subroutine *\w+)')
re_subroutine_yargs_find = re.compile(r' *subroutine *(\w+)')
re_subroutine_nargs_find = re.compile(r' *subroutine *(\w+)')
re_decl = re.compile(r'^ *(logical|character|integer|real|dimension|double precision)[*\d() ]*(.*)')
re_preamble = re.compile(r'^ *(use|implicit|data|common|parameter)')
re_space = re.compile(r'( *)\w')
re_format = re.compile(r' *[0-9]* FORMAT')
re_ignore = re.compile(r"^ *[!'/]")
# Edit each file in turn
for file in files:
    with open(file, 'r') as f:
        lines = f.readlines()

    local = []
    for i in range(len(lines)):
        if lines[i].strip().lower().startswith('subroutine'):
            d = 0
            uses_common = False
            for d in range(i, len(lines)):
                if 'use common' in lines[d].lower():
                    uses_common = True
                    break
                if lines[d].strip(' \n').lower() == 'end':
                    # End of subroutine reached
                    break

            if uses_common:
                # This function uses common variables
                lines[d] = re.sub(re_use_common, r'\1use mod_common', lines[d].lower())

                # Get a list of input variables this function uses
                i_start = lines[i].find('(')
                if i_start != -1:
                    _inputs = ''
                    # subroutine has input variables
                    for d in range(i, len(lines)):
                        l = lines[d].lower()
                        if d == i:
                            l = l[i_start + 1:]
                        _inputs += re.sub(re_brackets, '', l.strip(')&\n '))
                        if lines[d].strip().endswith(')'):
                            break

                    local = re.split(re_sep, _inputs)
                    lines[i] = re.sub(re_subroutine_yargs_sub, r'\1ctxt, ', lines[i].lower())
                    changed_subroutines += [(re.findall(re_subroutine_yargs_find, lines[i])[0], False)]
                else:
                    d = i
                    lines[i] = re.sub(re_subroutine_nargs_sub, r'\1(ctxt)', lines[i].lower())
                    changed_subroutines += [(re.findall(re_subroutine_nargs_find, lines[i])[0], True)]

                # Get a list of local variables declared by this function
                d_last = d+1
                for d in range(d+1, len(lines)):
                    m = re.search(re_decl, lines[d].lower())
                    if m is not None:
                        decl = re.sub(re_brackets, '', m.group(2))
                        local += re.split(re_sep, decl)
                    elif re.search(re_preamble, lines[d].lower()) is not None:
                        d_last = d
                    elif not lines[d].strip().startswith('!') and len(lines[d].strip()):
                        m = re.search(re_space, lines[d_last])
                        space = ''
                        if m is not None:
                            space = m.group(1)

                        lines[d_last] += space + 'type(Common), intent(inout) :: ctxt\n'
                        break

                # `local` now contains a list of local variables declared by this subroutine
                # now loop over all remaining lines of this subroutine and prefix variables used from common
                for d in range(d, len(lines)):
                    if lines[d].strip(' \n').lower() == 'end':
                        # End of subroutine reached
                        break

                    for param in re.findall(re_word, lines[d]):
                        if re.search(re_format, lines[d]) is None and re.search(re_ignore, lines[d]) is None:
                            if param.lower() in context:
                                lines[d] = re.sub(r"(?<!\w)(?<!ctxt%)(?<!')({})(?!\w)".format(param.lower()), r'ctxt%\1', lines[d].lower())

    with open(os.path.join(output_dir, file), 'w') as f:
        f.writelines(lines)

re_call_nargs = re.compile(r'call( *\w+)')
re_call_yargs = re.compile(r'call( *\w+\()')
re_caps = re.compile(r'([A-Z]+)(?![a-z])')


def repl_caps(m):
    return m.group(1).lower()


for file in files:
    fname = os.path.join(output_dir, file)
    with open(fname, 'r') as f:
        lines = f.readlines()

    for i in range(len(lines)):
        m = re.search(r'call *(\w+)', lines[i].lower())
        if m is not None:
            for changed_subroutine in changed_subroutines:
                if m.group(1).lower() == changed_subroutine[0]:
                    if changed_subroutine[1]:
                        lines[i] = re.sub(re_call_nargs, r'call\1(ctxt)', lines[i].lower())
                    else:
                        lines[i] = re.sub(re_call_yargs, r'call\1ctxt, ', lines[i].lower())

        lines[i] = re.sub(re_caps, repl_caps, lines[i])

    with open(fname, 'w') as f:
        f.writelines(lines)

print('done')
