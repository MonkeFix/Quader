namespace Quader.Engine
{
    public enum BoardCellType
    {
        None = 0, I, O, T, L, J, S, Z, 
        /// <summary>
        /// Garbage block
        /// </summary>
        Garbage, 
        /// <summary>
        /// Solid lines that cannot be cleared
        /// </summary>
        Solid, 
        /// <summary>
        /// Determines whether player can drop another piece or he will immediately lose
        /// </summary>
        Failing
    }
}