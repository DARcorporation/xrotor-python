import regex as re
import glob
import os
import typing

output_dir = 'output'
if not os.path.isdir(output_dir):
    os.mkdir(output_dir)
else:
    for file in os.listdir(output_dir):
        os.remove(os.path.join(output_dir, file))

do_write = False


# Make a list of the functions in each module
# Make a list of which functions each function uses
# Remove all using statements
functions = dict()
files = [file for file in glob.glob('*.f90')]
for file in files:
    with open(file, 'r') as f:
        lines = f.readlines()

    new_lines = []
    current_module = None
    current_function = None
    for i in range(len(lines)):
        new_lines.append(lines[i]
                         )
        match = re.match(r'^ *module (\w+)', lines[i].lower())
        if match:
            current_module = match.group(1)
            functions.update({current_module: dict()})
            continue

        match = re.match(r'^ *(function|subroutine) +(\w+)', lines[i].lower())
        if match:
            current_function = match.group(2)

            if current_module is not None:
                functions[current_module].update({current_function: set()})
            continue

        if current_function is not None:
            match = re.match(r'^ *use (\w+)', lines[i].lower())
            if match:
                if match.group(1) != 'm_common':
                    del new_lines[-1]
                continue

            match = re.match(r'[^!]*call *(\w+)', lines[i].lower())
            if match:
                functions[current_module][current_function].add(match.group(1))
                continue

    if do_write:
        with open(os.path.join(output_dir, os.path.split(file)[-1]), 'w') as f:
            f.writelines(new_lines)

# Clean up modules without functions, and add modules of functions that are used
for mod in list(functions.keys()):
    if not len(functions[mod]):
        del functions[mod]
        continue

for mod in list(functions.keys()):
    for fcn in list(functions[mod].keys()):
        new_uses = dict()
        for uses_fcn in functions[mod][fcn]:
            for sub_mod, sub_fcns in functions.items():
                if uses_fcn in sub_fcns and sub_mod != mod:
                    if sub_mod in new_uses:
                        new_uses[sub_mod].add(uses_fcn)
                    else:
                        new_uses.update({sub_mod: {uses_fcn}})

        functions[mod][fcn] = new_uses

if do_write:
    # Add (specific) use statements
    files = [os.path.join(output_dir, file) for file in glob.glob('*.f90')]
    for file in files:
        with open(file, 'r') as f:
            lines = f.readlines()

        new_lines = []
        current_module = None
        current_function = None
        i = -1
        while i < len(lines)-1:
            i += 1

            new_lines.append(lines[i])
            match = re.match(r'^ *module (\w+)', lines[i].lower())
            if match:
                current_module = match.group(1)
                continue

            match = re.match(r'^ *(function|subroutine) +(\w+)', lines[i].lower())
            if match:
                current_function = match.group(2)

                if current_module is not None:
                    count = lines[i].count('(') - lines[i].count(')')
                    while count != 0:
                        i += 1
                        new_lines.append(lines[i])
                        count += lines[i].count('(') - lines[i].count(')')

                    for uses_module, uses_functions in functions[current_module][current_function].items():
                        new_lines.append(f'        use {uses_module}, only: {", ".join(uses_functions)}\n')
                continue

        with open(file, 'w') as f:
            f.writelines(new_lines)


# Detect circular references
def find_dep(chains: typing.List[typing.List[str]]):
    for uses_mod, uses_fcns in functions[chains[-1][-2]][chains[-1][-1]].items():
        for uses_fcn in uses_fcns:
            chain_base = chains[-1].copy()
            chains[-1].extend([uses_mod, uses_fcn])
            if uses_mod not in chains[-1][:-2]:
                chains = find_dep(chains)
            chains.append(chain_base)

    return chains


chains = []
for mod, fcns in functions.items():
    for fcn in fcns.keys():
        new_chains = find_dep([[mod, fcn]])
        for i in range(len(new_chains)):
            if new_chains[i][0] in new_chains[i][2:]:
                already_found = False
                for j in range(int(len(new_chains[i])/2)):
                    new_chains[i] = new_chains[i][2:] + new_chains[i][:2]
                    if new_chains[i] in chains:
                        already_found = True
                        break

                if not already_found:
                    chains.append(new_chains[i])


exit(0)


