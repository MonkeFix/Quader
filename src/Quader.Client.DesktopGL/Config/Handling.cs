using Nez.Persistence;

namespace Quader.Config
{
    public class Handling : IGameConfigEntry<Handling>
    {
        /// <summary>
        /// Gets or sets ARR. The speed at which pieces move when holding down movement keys, measured in milliseconds
        /// </summary>
        [JsonInclude]
        public float AutomaticRepeatRate { get; set; } = 0f;

        /// <summary>
        /// Gets or sets DAS. The time between the initial keypress and the start of its automatic repeat movement, measured in milliseconds
        /// </summary>
        [JsonInclude]
        public float DelayedAutoShift { get; set; } = 128f;

        /// <summary>
        /// Gets or sets DAS. If not 0, any ongoing DAS movement will pause for a set amount of time after dropping/rotating a piece, measured in milliseconds
        /// </summary>
        [JsonInclude]
        public float DasCutDelay { get; set; } = 0f;

        /// <summary>
        /// Gets or sets SDF. The factor with which soft drops change the gravity speed
        /// </summary>
        [JsonInclude]
        public int SoftDropFactor { get; set; } = int.MaxValue;

        public Handling Defaults()
        {
            return new Handling();
        }
    }
}