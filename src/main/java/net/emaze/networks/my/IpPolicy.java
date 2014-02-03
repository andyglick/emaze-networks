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

    Ranges<Ip> getRanges();

    FixedSizeNatural maxValue();

    FixedSizeNatural minValue();

    int maxPopulation();

    int compare(Ip lhs, Ip rhs);

    int compare(Mask lhs, Mask rhs);

    IpPolicy selectForComparison(IpPolicy other);

    boolean acceptSize(FixedSizeNatural bits);

    String toCanonicalString(FixedSizeNatural bits);

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
        public boolean acceptSize(FixedSizeNatural bits) {
            return bits.length() == IPV6_BITS;
        }

        @Override
        public String toCanonicalString(FixedSizeNatural bits) {
            final StringBuilder builder = new StringBuilder();
            final byte[] octets = bits.toByteArray();
            builder.append(String.format("%02x%02x", octets[0] & 0xFF, octets[1] & 0xFF));
            for (int index = 2; index != octets.length; index += 2) {
                builder.append(String.format(":%02x%02x", octets[index] & 0xFF, octets[index + 1] & 0xFF));
            }
            return builder.toString();
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
        public boolean acceptSize(FixedSizeNatural bits) {
            return bits.length() == IPV4_BITS;
        }

        @Override
        public String toCanonicalString(FixedSizeNatural bits) {
            final byte[] octets = bits.toByteArray();
            final int first = octets[0] & 0xFF;
            final int second = octets[1] & 0xFF;
            final int third = octets[2] & 0xFF;
            final int fourth = octets[3] & 0xFF;
            return String.format("%s.%s.%s.%s", first, second, third, fourth);
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
