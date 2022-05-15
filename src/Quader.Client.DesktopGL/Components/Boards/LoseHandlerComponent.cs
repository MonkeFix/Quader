using System;
using Nez;
using Quader.Engine;

namespace Quader.Components.Boards
{
    public class LoseHandlerComponent : Component, IBoardComponent
    {
        public Board Board { get; }

        private readonly Action _restartAction;

        public LoseHandlerComponent(Board board, Action restartAction)
        {
            Board = board;
            _restartAction = restartAction;

            Board.PieceCannotBeSpawned += BoardOnPieceCannotBeSpawned;
        }

        public void Restart()
        {
            Console.WriteLine("RESTART");
        }

        private void BoardOnPieceCannotBeSpawned(object? sender, EventArgs e)
        {
            Console.WriteLine("CANNOT SPAWN A NEW PIECE!");
            Board.Reset();
            _restartAction.Invoke();
        }
    }
}