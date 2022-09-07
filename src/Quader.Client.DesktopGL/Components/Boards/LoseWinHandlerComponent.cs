using System;
using Nez;
using Quader.Engine;

namespace Quader.Components.Boards
{
    public class LoseWinHandlerComponent : Component, IBoardComponent
    {
        public Board Board { get; }

        public LoseWinHandlerComponent(Board board)
        {
            Board = board;

            Board.PieceCannotBeSpawned += BoardOnPieceCannotBeSpawned;
        }

        private void BoardOnPieceCannotBeSpawned(object? sender, EventArgs e)
        {
            //Console.WriteLine("CANNOT SPAWN A NEW PIECE!");
            //Board.Reset();
        }
    }
}