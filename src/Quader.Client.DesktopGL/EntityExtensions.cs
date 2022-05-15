using System.Collections.Generic;
using System.Linq;
using Nez;

namespace Quader
{
    public static class EntityExtensions
    {
        public static IEnumerable<T> AddComponents<T>(this Entity entity, IEnumerable<T> components) where T : Component
        {
            var cl = components.ToList();

            foreach (var component in cl)
            {
                entity.AddComponent(component);
            }

            return cl;
        }
    }
}