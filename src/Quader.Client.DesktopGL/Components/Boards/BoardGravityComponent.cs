using Nez;
using Quader.Engine;

namespace Quader.Components.Boards
{
    public class BoardGravityComponent : Component, IUpdatable, IBoardComponent
    {
        public Board Board { get; }

        public BoardGravityComponent(Board board)
        {
            Board = board;
        }

        public void Update()
        {
            Board.UpdateGravity(Time.DeltaTime);
        }
    }
}