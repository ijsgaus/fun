using System;
using System.Collections.Concurrent;
using System.Linq;
using System.Reflection;

namespace Fun.Ast
{
    public static class CommonExtensions
    {
        public static FieldInfo GetFieldInfo(this Enum en)
        {
            var t = en.GetType();
            var name = Enum.GetName(t, en);
            return name == null ? null : t.GetRuntimeField(name);
        }

        


        private static readonly ConcurrentDictionary<Keyword, string> _keywordCache = new ConcurrentDictionary<Keyword, string>();

        public static string ToCode(this Keyword keyword)
        {
            return _keywordCache.GetOrAdd(keyword, p => RepresentationAttribute.GetData(p.GetFieldInfo()));
        }

        public static T GetCustomAttribute<T>(this ICustomAttributeProvider provider)
            where T : Attribute
        {
            return provider.GetCustomAttributes(typeof(T), true).Cast<T>().FirstOrDefault();
        }
    }
}