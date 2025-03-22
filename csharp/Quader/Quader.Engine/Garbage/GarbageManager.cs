using Quader.Engine.Boards;
using Quader.Engine.Collections;

namespace Quader.Engine.Garbage;

public class GarbageManager
{
    public Deque<IncomingDamage> Queue { get; } = new();

    private Random _rng = new Random();
    private int? LastGarbageX = null;
    private AttackSettings _attackSettings;

    public GarbageManager(AttackSettings attackSettings)
    {
        _attackSettings = attackSettings;
    }

    public void Attack(int width, int damage)
    {
        if (damage <= 0) return;

        var holeX = _rng.Next(0, (int)width);
        Queue.AddBack(new IncomingDamage(damage, (int)_attackSettings.GarbageDelayMs, (uint)holeX));
    }

    public void PushGarbage(int amount, int messiness, CellHolder cellHolder)
    {
        int garbageHoleX;
        if (LastGarbageX.HasValue)
        {
            // TODO: Use messiness
            garbageHoleX = LastGarbageX.Value;
        }
        else
        {
            garbageHoleX = _rng.Next(0, cellHolder.Width);
        }

        for (int i = 0; i < amount; i++)
        {
            cellHolder.PushGarbage(garbageHoleX);
        }
    }

    public void PushGarbageAt(int amount, int holeX, CellHolder cellHolder)
    {
        for (int i = 0; i < amount; i++)
        {
            cellHolder.PushGarbage(holeX);
        }
    }

    public GarbageHardDropResult HardDrop(int linesCleared, int outgoingDamage)
    {
        var result = new GarbageHardDropResult(new List<IncomingDamage>(), outgoingDamage);

        // if the queue is empty then deal damage to the enemies
        if (Queue.Count == 0) return result;

        // accumulate total incoming damage
        var incomingDmg = new Deque<IncomingDamage>();
        while (Queue.Count > 0)
        {
            var recvDmg = Queue.Last(); // front
            if (recvDmg.Delay > 0) break;

            recvDmg = Queue.RemoveFront();
            incomingDmg.AddFront(recvDmg);
        }

        // if player hasn't cleared any lines, then push garbage onto his board
        if (linesCleared == 0)
        {
            return new GarbageHardDropResult(incomingDmg.ToList(), 0);
        }

        while (incomingDmg.Count > 0)
        {
            var recvDmg = incomingDmg.RemoveFront();
            if (recvDmg.Amount <= result.OutDamage)
            {
                result.OutDamage -= recvDmg.Amount;
            }
            else
            {
                recvDmg.Amount -= result.OutDamage;
                incomingDmg.AddFront(recvDmg);

                result.OutDamage = 0;

                foreach (var dmg in incomingDmg)
                {
                    Queue.AddFront(dmg);
                }

                return result;
            }
        }

        // see if queue still contains damage entries with delay > 0
        while (Queue.Count > 0)
        {
            var dmg = Queue.RemoveFront();
            result.OutDamage -= dmg.Amount;
            if (result.OutDamage == 0)
                break;

            if (result.OutDamage < 0)
            {
                Queue.AddFront(new IncomingDamage(-result.OutDamage, dmg.Delay, dmg.HoleX));
                result.OutDamage = 0;
                break;
            }
        }

        return result;
    }

    public void Update(TimeManager timeManager)
    {
        foreach (var dmg in Queue)
        {
            dmg.Update(timeManager);
        }
    }

    public void Reset()
    {
        Queue.Clear();
        LastGarbageX = null;
    }
}