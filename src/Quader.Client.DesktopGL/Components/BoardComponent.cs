using System;
using Microsoft.Xna.Framework;
using Nez;
using Random = Nez.Random;

namespace Quader.Components
{
    public class BoardComponent : RenderableComponent
    {
        public override float Width => 1000;
        public override float Height => 1000;

        public int BoardWidth => 10;
        public int BoardHeight => 40;
        

        private bool[] _boardState = new bool[400];

        public BoardComponent()
        {
            for (int i = 0; i < 400; i++)
            {
                if (Random.RNG.Next(0, 2) == 0)
                    _boardState[i] = true;
            }
        }

        public override void Render(Batcher batcher, Camera camera)
        {
            for (int x = 0; x < BoardWidth; x++)
            {
                for (int y = 0; y < BoardHeight; y++)
                {
                    var index = x * BoardWidth + y;
                    if (_boardState[index])
                        batcher.DrawRect(x * 32, y * 32, 32, 32, Color.Red);
                    else 
                        batcher.DrawRect(x * 32, y * 32, 32, 32, Color.White);
                }
            }
        }

        public void Set(int index)
        {
            _boardState[index] = true;
        }

        public void Set(int x, int y)
        {
            var index = x * BoardWidth + y;
            Set(index);
        }

        public void Unset(int index)
        {
            _boardState[index] = false;
        }

        public void Unset(int x, int y)
        {
            var index = x * BoardWidth + y;
            _boardState[index] = false;
        }
    }
}