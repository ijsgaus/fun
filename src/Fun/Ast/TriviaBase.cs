namespace Fun.Ast
{
    public abstract class TriviaBase<T> : IHasWidth
    {
        public TriviaBase(T kind, uint width)
        {
            Kind = kind;
            Width = width;
        }

        public T Kind { get; }
        public uint Width { get; }
    }
}