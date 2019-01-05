import json
from dataclasses import dataclass
from copy import deepcopy

@dataclass()
class Group:
    count: int
    hp: int
    ap: int
    attack_type: str
    initiative: int
    weakness: [str]
    immunities: []
    name: str
    army: str

    @staticmethod
    def from_json(obj):
        group = Group(count=obj['count'],
                      hp=obj['hp'],
                      ap=obj['attackPower'],
                      attack_type=obj['attackType'],
                      initiative=obj['initiative'],
                      name='',
                      army='',
                      immunities=[],
                      weakness=[])

        if 'types' in obj:
            group.weakness = obj['types']['weak']
            group.immunities = obj['types']['immune']

        return group

    def effective_power(self):
        return self.ap * self.count

    def calc_damage(self, other):
        if self.attack_type in other.immunities:
            return 0
        elif self.attack_type in other.weakness:
            return self.effective_power() * 2
        else:
            return self.effective_power()

    def attack(self, other):
        damage = self.calc_damage(other)
        units_killed = damage // other.hp

        if other.count <= units_killed:
            units_killed = other.count
            other.count = 0
        else:
            other.count -= units_killed

        return units_killed

    def select_target(self, options):
        for x in options:
            print_target(self, x)
        sorted_options = sorted(options, key=lambda x: (self.calc_damage(x), x.effective_power(), x.initiative),
                                reverse=True)

        if not sorted_options:
            return None
        else:
            choice = sorted_options[0]

        if self.calc_damage(choice) == 0:
            return None
        else:
            return choice

    def __hash__(self):
        return hash((self.attack_type, self.name, self.army, self.initiative, self.ap))


def find_weaknesses(block, g):
    if len(block) < 2: return
    block = block.replace(",", "").replace(";", "")
    weak = False
    for word in block.split(" "):
        if word == "to": continue
        if word == "weak":
            weak = True
            continue
        if word == "immune":
            weak = False
            continue
        if weak:
            g.weakness.append(word)
        else:
            g.immunities.append(word)


# tried manually parsing json and that gave incorrect answer so stole someone's parsing
def get_and_parse_input(filename):
    immunes = []
    infections = []
    group_id = 0
    with open(filename, "r") as f:
        imm = False
        for line in f.readlines():
            line = line.strip()
            if line.strip() == "": continue
            if line == "Immune System:":
                imm = True
                continue
            if line == "Infection:":
                group_id = 0
                imm = False
                continue
            pieces = line.split(" ")
            g = Group(count=0,
                      hp=0,
                      ap=0,
                      attack_type='',
                      initiative=0,
                      name='',
                      army='',
                      immunities=[],
                      weakness=[])
            g.name = group_id
            g.count = int(pieces[0])
            g.hp = int(pieces[4])
            g.initiative = int(pieces[-1])

            # find attack type/dmg
            j = line.find("attack that does ")
            k = line.find(" ", j + 17)
            g.ap = int(line[j + 17:k])
            l = line.find(" ", k + 1)
            g.attack_type = line[k + 1:l]
            if line.find("(") > 0:
                find_weaknesses(line[line.find("(") + 1:line.find(")")], g)
            if imm:
                immunes.append(g)
            else:
                infections.append(g)
            group_id += 1

    for i, x in enumerate(immunes):
        x.name = f'{i + 1}'
        x.army = 'Immune System'

    for i, x in enumerate(infections):
        x.name = f'{i + 1}'
        x.army = 'Infection'

    return immunes, infections


def get_input(filename):
    with open(filename) as f:
        raw = json.load(f)
        immuneRaw = raw['immune']
        infectionRaw = raw['infection']

        immune = list(map(Group.from_json, immuneRaw))
        infection = list(map(Group.from_json, infectionRaw))

        for i, x in enumerate(immune):
            x.name = f'{i + 1}'
            x.army = 'Immune System'

        for i, x in enumerate(infection):
            x.name = f'{i + 1}'
            x.army = 'Infection'

        return immune, infection

should_print = False


def print_starting_stats(army):
    if should_print:
        if not army:
            print('No groups remain.')
        else:
            print(f'{army[0].army}:')
            for x in army:
                print(f'Group {x.name} contains {x.count} units')


def print_target(a, b):
    if b and should_print:
        print(f'{a.army} group {a.name} would deal defending group {b.name} {a.calc_damage(b)} damage')


def print_attack(a, b, killed):
    if b and should_print:
        print(f'{a.army} group {a.name} attacks defending group {b.name}, killing {killed} units')


def run_round(immune, infection):
    total_killed = 0
    immune_targets = set(immune)
    infection_targets = set(infection)

    selections = []

    print_starting_stats(immune)
    print_starting_stats(infection)

    if should_print: print('')

    # Targeting
    for g in sorted(infection, key=lambda x: (x.effective_power(), x.initiative), reverse=True):
        s = g.select_target(list(immune_targets))
        if s:
            immune_targets.remove(s)
        selections.append((g, s))

    for g in sorted(immune, key=lambda x: (x.effective_power(), x.initiative), reverse=True):
        s = g.select_target(list(infection_targets))
        if s:
            infection_targets.remove(s)
        selections.append((g, s))

    if should_print: print('')

    # Attacking
    for g, o in sorted(selections, key=lambda x: x[0].initiative, reverse=True):
        if o:
            killed = g.attack(o)
            total_killed += killed
            print_attack(g, o, killed)

    new_immune = [x for x in immune if x.count > 0]
    new_infection = [x for x in infection if x.count > 0]

    # print('\n ----- ----- -----\n')

    return new_immune, new_infection, total_killed


def part_one(filename):
    immune, infection = get_and_parse_input(filename)
    while len(immune) * len(infection) > 0:
        immune, infection, _ = run_round(immune, infection)

    print_starting_stats(immune)
    print_starting_stats(infection)

    if not infection:
        print('Immune Won')
        print(f'part 1: {sum([x.count for x in immune])}')

    if not immune:
        print('Infection Won')
        print(f'part 1: {sum([x.count for x in infection])}')


def part_two(filename):
    immune, infection = get_and_parse_input(filename)
    boost = 0
    cur_immune = []
    stalemate = False
    while not cur_immune or stalemate:
        cur_immune = deepcopy(immune)
        cur_infection = deepcopy(infection)
        print(f'Boost: {boost}')
        for x in cur_immune:
            x.ap += boost
        stalemate = False
        while (len(cur_immune) > 0 and len(cur_infection) > 0) and not stalemate:
            cur_immune, cur_infection, total_kills = run_round(cur_immune, cur_infection)
            stalemate = total_kills == 0
            if stalemate:
                print('Stalemate!')

        boost += 1

    print(f'Part 2: {sum([x.count for x in cur_immune])}')

def main():
    test_file = "../inputs/input-24.0.txt"
    real_file = "../inputs/input-24.1.txt"
    part_two(real_file)


main()
