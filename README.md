# dungeon

My attempt to learn myself a Haskell by making something like a roguelike.

<img width="588" height="500" alt="dungeon" src="https://github.com/user-attachments/assets/4b9d7ca5-49e8-46d6-9641-db43f316b154" />

## Running

Start the ordinary Vty game:

```sh
stack run
```

The line-oriented console frontend controls the lowest-ID NPC:

```sh
stack run -- --console
```

Hybrid mode keeps the Vty player interface and gives one NPC to a child
process. NPC identities are stable numeric keys in the initial layout; the
test dugeon's adder is NPC 0. For example, this launches the included agent
and makes the adder patrol horizontally between x coordinates 8 and 12:

```sh
stack run -- --agent 0 -- python3 scripts/patrol_agent.py 8 12
```

The child uses the same console protocol as `--console`.  Its standard
output sends commands and its standard input receives responses. The
child process-controlled NPC makes one decision whenever the Vty player
submits a gameplay turn. If the child exits or closes its pipe, the
NPC resumes its built-in behaviour. Movement uses `h`, `j`, `k`, and
`l`; `.` spends the turn without moving the NPC.
