class Node:    
    def __init__(self, name: str, weight: int):
        self.name = name
        self.weight = weight
        self.subtreeweight = 0
        self.outgoing = []
        self.incoming = []

    def __hash__(self):
        return hash(self.name)

    def __eq__(self, other):
        if isinstance(self, other.__class__):
            return self.name == other.name
        return False

    def __repr__(self):
        return '<Node %s (%d)>' % (self.name, self.weight)

def parseNodes(inputFile: str):
    nodes = dict()
    with open(inputFile, 'r') as f:
        for line in f:
            nName = line.split(' (')[0]
            nWeight = int(line.split('(')[1].split(')')[0])
            nodes[nName] = Node(nName, nWeight)
            
    return nodes

def parseEdges(nodes, inputFile: str):
    with open(inputFile, 'r') as f:
        for line in f:
            sects = line.split(' -> ')
            curNodeName = line.split(' (')[0]
            if len(sects) == 2:
                names = sects[1].split(', ')
                for name in names:
                    nodes[curNodeName].outgoing.append(nodes[name.strip()])
                    

def topoSortRecurse(node, stack, visited):
    subweight = 0
    for nextNode in node.outgoing:
        subweight += topoSortRecurse(nextNode, stack, visited)
    stack.append(node)
    visited.add(node)
    node.subtreeweight = subweight
    subweight += node.weight
    return subweight
                    
def topoSort(nodes):
    stack = []
    visited = set()
    for key in nodes:
        if nodes[key] not in visited:
            topoSortRecurse(nodes[key], stack, visited)
    stack.reverse()
    return stack

def treerep(node):
    return (node, [treerep(n) for n in node.outgoing])

def findImbalance(node):
    subweights = dict()
    subweightcounts = dict()
    for n in node.outgoing:
        sw = n.weight + n.subtreeweight
        subweights[n] = sw
        subweightcounts[sw] = subweightcounts.get(sw, 0) + 1
    if len(subweightcounts.keys()) == 1:
        # No imbalance in subtrees; if there is one, it's this node
        return node
    else:
        # Imbalance - there should be a count with exactly 1 entry
        for ct in subweightcounts:
            if subweightcounts[ct] == 1:
                oddCountOut = ct
                break
        for n in subweights:
            if subweights[n] == oddCountOut:
                # Found the odd node out
                return findImbalance(n)

        
def findNeededForBalance(root, imba):
    if imba in root.outgoing:
        current = imba.weight
        currentTotal = imba.weight + imba.subtreeweight
        # Find another one
        for n in root.outgoing:
            if n != imba:
                requiredTotal = n.weight + n.subtreeweight
                break
        diff = requiredTotal - currentTotal
        fixedVal = current + diff
        return (imba, fixedVal)
    else:
        for n in root.outgoing:
            b = findNeededForBalance(n, imba) 
            if b is not None:
                return b
        return None

nodes = parseNodes('./data/day7.in')
parseEdges(nodes, './data/day7.in')
ordering = topoSort(nodes)
print("First node is: " + ordering[0].name)
tree = treerep(ordering[0])
imba = findImbalance(ordering[0])
(_, fixedVal) = findNeededForBalance(ordering[0], imba)
print("Fixed value is: " + str(fixedVal))