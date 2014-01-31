package net.emaze.networks.my;

import net.emaze.dysfunctional.Ranges;
import net.emaze.dysfunctional.contracts.dbc;
import net.emaze.dysfunctional.options.Maybe;
import net.emaze.dysfunctional.order.ComparableComparator;
import net.emaze.dysfunctional.order.SequencingPolicy;

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
