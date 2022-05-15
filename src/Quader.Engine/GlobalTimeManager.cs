using System;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;

namespace Quader.Engine
{
    public class TimeData
    {
        public string MethodName { get; set; }
        public TimeSpan LastTime { get; set; }
        public TimeSpan MeanTime { get; set; }

        private List<TimeSpan> AllTimes { get; } = new ();
        public double _average;
        public double _min;
        public double _max;

        public void Add(TimeSpan ts)
        {
            AllTimes.Add(ts);
            Recalc();
        }

        private void Recalc()
        {
            _average = AllTimes.Select(span => span.TotalMilliseconds).Average();
            _min = AllTimes.Select(span => span.TotalMilliseconds).Min();
            _max = AllTimes.Select(span => span.TotalMilliseconds).Max();
        }

        public override string ToString()
        {
            return $"{MethodName}:    Last: {LastTime.TotalMilliseconds:F6} |" +
                   $" Mean: {_average:F6} ({AllTimes.Count}) " +
                   $"Min: {_min:F6} | Max: {_max}";
        }
    }
    
    public static class GlobalTimeManager
    {
        public static Dictionary<string, TimeData> TimeData { get; } = new ();

        public static void AddData(string methodName, TimeSpan lastTime)
        {
            if (!TimeData.ContainsKey(methodName))
            {
                TimeData[methodName] = new TimeData
                {
                    MethodName = methodName,
                    LastTime = lastTime
                };
                
                TimeData[methodName].Add(lastTime);
            }
            else
            {
                TimeData[methodName].LastTime = lastTime;
                TimeData[methodName].Add(lastTime);
            }
        }
    }
}