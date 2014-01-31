package net.emaze.networks.my;

import net.emaze.dysfunctional.Ranges;
import net.emaze.dysfunctional.contracts.dbc;
import net.emaze.dysfunctional.options.Maybe;
import net.emaze.dysfunctional.order.ComparableComparator;
import net.emaze.dysfunctional.order.SequencingPolicy;
import net.emaze.dysfunctional.tuples.Pair;

public interface IpPolicy extends SequencingPolicy<Ip> {

    public static final FixedSizeNatural IPV4_TO_V6_PREFIX = FixedSizeNatural.biggest(16).extendTo(128).shiftLeft(32);
    public static final FixedSizeNatural IPV4_TO_V6_MASK = FixedSizeNatural.biggest(128).shiftLeft(32);
    public static final int IPV4_TO_V6_POPULATION = 96;

    Ip getFirstIp();

    Ip getLastIp();

    Mask getNarrowestMask();

    Mask getWidestMask();

    Mask mask(int size);

    Ranges<Ip> getRanges();

    FixedSizeNatural maxValue();

    FixedSizeNatural minValue();

    int maxPopulation();

    int compare(Ip lhs, Ip rhs);

    int compare(Mask lhs, Mask rhs);

    IpPolicy selectForComparison(IpPolicy other);

    public static class V6 implements IpPolicy {

        public static final int IPV6_BITS = 128;
        private static final int MAX_MASK_POPULATION = 128;
        private static final FixedSizeNatural MIN_ADDRESS_IN_BITS = FixedSizeNatural.zero(IPV6_BITS);
        private static final FixedSizeNatural MAX_ADDRESS_IN_BITS = FixedSizeNatural.biggest(IPV6_BITS);

        @Override
        public Ip getFirstIp() {
            return new Ip(MIN_ADDRESS_IN_BITS, new V6());
        }

        @Override
        public Ip getLastIp() {
            return new Ip(MAX_ADDRESS_IN_BITS, new V6());
        }

        @Override
        public Mask getNarrowestMask() {
            return new Mask(MAX_MASK_POPULATION, new V6());
        }

        @Override
        public Mask mask(int size) {
            return new Mask(size, this);
        }

        @Override
        public Mask getWidestMask() {
            return new Mask(0, new V6());
        }

        @Override
        public Ranges<Ip> getRanges() {
            return new Ranges<>(new ComparableComparator<Ip>(), new V6(), this.getFirstIp());
        }

        @Override
        public FixedSizeNatural maxValue() {
            return MAX_ADDRESS_IN_BITS;
        }

        @Override
        public FixedSizeNatural minValue() {
            return MIN_ADDRESS_IN_BITS;
        }

        @Override
        public int compare(Ip lhs, Ip rhs) {
            final FixedSizeNatural left = lhs.version() instanceof V6 ? lhs.bits() : IPV4_TO_V6_PREFIX.or(lhs.bits().extendTo(IPV6_BITS));
            final FixedSizeNatural right = rhs.version() instanceof V6 ? rhs.bits() : IPV4_TO_V6_PREFIX.or(rhs.bits().extendTo(IPV6_BITS));
            return left.compareTo(right);
        }

        @Override
        public int compare(Mask lhs, Mask rhs) {
            final FixedSizeNatural left = lhs.version() instanceof V6 ? lhs.bits() : IPV4_TO_V6_PREFIX.or(lhs.bits().extendTo(IPV6_BITS));
            final FixedSizeNatural right = rhs.version() instanceof V6 ? rhs.bits() : IPV4_TO_V6_PREFIX.or(rhs.bits().extendTo(IPV6_BITS));
            return left.compareTo(right);
        }

        @Override
        public IpPolicy selectForComparison(IpPolicy other) {
            return this;
        }

        public static Ip toV6(Ip source) {
            if (source.version() instanceof V6) {
                return source;
            }
            final FixedSizeNatural address = IPV4_TO_V6_PREFIX.or(source.bits().extendTo(IPV6_BITS));
            return new Ip(address, new V6());
        }

        public static Mask toV6(Mask source) {
            if (source.policy instanceof V6) {
                return source;
            }
            return new Mask(source.population() + IPV4_TO_V6_POPULATION, new IpPolicy.V6());
        }

        public static Network toV6(Network source) {
            if (source.version() instanceof V6) {
                return source;
            }
            final Pair<Ip, Mask> cidr = source.toCidr();
            return new Network(toV6(cidr.first()), toV6(cidr.second()));
        }

        @Override
        public int maxPopulation() {
            return MAX_MASK_POPULATION;
        }

        @Override
        public Maybe<Ip> next(Ip ip) {
            if (this.getLastIp().equals(ip)) {
                return Maybe.nothing();
            }
            return Maybe.just(ip.next());
        }

        @Override
        public boolean equals(Object other) {
            return other instanceof V6;
        }

        @Override
        public int hashCode() {
            return V6.class.hashCode();
        }
    }

    public static class V4 implements IpPolicy {

        private static final int IPV4_BITS = 32;
        private static final int MAX_MASK_POPULATION = 32;
        private static final FixedSizeNatural MIN_ADDRESS_IN_BITS = FixedSizeNatural.zero(IPV4_BITS);
        private static final FixedSizeNatural MAX_ADDRESS_IN_BITS = FixedSizeNatural.biggest(IPV4_BITS);

        @Override
        public Ip getFirstIp() {
            return new Ip(MIN_ADDRESS_IN_BITS, this);
        }

        @Override
        public Ip getLastIp() {
            return new Ip(MAX_ADDRESS_IN_BITS, this);
        }

        @Override
        public Mask getNarrowestMask() {
            return new Mask(MAX_MASK_POPULATION, this);
        }

        @Override
        public Mask getWidestMask() {
            return new Mask(0, this);
        }

        @Override
        public Mask mask(int size) {
            return new Mask(size, new IpPolicy.V4());
        }

        @Override
        public Ranges<Ip> getRanges() {
            return new Ranges<>(new ComparableComparator<Ip>(), this, this.getFirstIp());
        }

        @Override
        public FixedSizeNatural maxValue() {
            return MAX_ADDRESS_IN_BITS;
        }

        @Override
        public FixedSizeNatural minValue() {
            return MIN_ADDRESS_IN_BITS;
        }

        @Override
        public int compare(Ip lhs, Ip rhs) {
            dbc.precondition((lhs.version() instanceof V4) && (rhs.version() instanceof V4), "Use V6 to compare V4 with V6");
            return lhs.bits().compareTo(rhs.bits());
        }

        @Override
        public int compare(Mask lhs, Mask rhs) {
            dbc.precondition((lhs.version() instanceof V4) && (rhs.version() instanceof V4), "Use V6 to compare V4 with V6");
            return lhs.bits().compareTo(rhs.bits());
        }

        @Override
        public IpPolicy selectForComparison(IpPolicy other) {
            if (other instanceof V4) {
                return this;
            }
            return other;
        }

        public static Ip toV4(Ip source) {
            if (source.version() instanceof V4) {
                return source;
            }
            dbc.precondition(source.bits().and(IPV4_TO_V6_MASK).equals(IPV4_TO_V6_PREFIX), "Address cannot be converted to IPv4");
            final FixedSizeNatural address = source.bits().extendTo(V6.IPV6_BITS).and(IPV4_TO_V6_MASK.not());
            return new Ip(address, new V4());
        }

        public static Mask toV4(Mask source) {
            if (source.policy instanceof V4) {
                return source;
            }
            dbc.precondition(source.population() >= IPV4_TO_V6_POPULATION, "Mask cannot be converted to IPv4");
            return new Mask(source.population() - IPV4_TO_V6_POPULATION, new IpPolicy.V4());
        }

        public static Network toV4(Network source) {
            if (source.version() instanceof V4) {
                return source;
            }
            final Pair<Ip, Mask> cidr = source.toCidr();
            return new Network(toV4(cidr.first()), toV4(cidr.second()));
        }

        @Override
        public int maxPopulation() {
            return MAX_MASK_POPULATION;
        }

        @Override
        public Maybe<Ip> next(Ip ip) {
            if (this.getLastIp().equals(ip)) {
                return Maybe.nothing();
            }
            return Maybe.just(ip.next());
        }

        @Override
        public boolean equals(Object other) {
            return other instanceof V4;
        }

        @Override
        public int hashCode() {
            return V4.class.hashCode();
        }
    }
}
