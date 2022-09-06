using Nez;
using Quader.Engine;

namespace Quader.Components.Boards
{
    public class BoardGravityComponent : Component, IUpdatable, IBoardComponent, IBoardToggleable
    {
        public Board Board { get; }

        public BoardGravityComponent(Board board)
        {
            Board = board;
        }

        public void Enable()
        {
            Enabled = true;
        }

        public void Disable()
        {
            Enabled = false;
        }

        public void Toggle()
        {
            Enabled = !Enabled;
        }

        public void Update()
        {
            Board.UpdateGravity(Time.DeltaTime);
        }
    }
}