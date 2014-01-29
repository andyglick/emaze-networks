package net.emaze.networks.my;

import java.math.BigInteger;
import java.util.Comparator;
import net.emaze.dysfunctional.Ranges;
import net.emaze.dysfunctional.contracts.dbc;
import net.emaze.dysfunctional.options.Maybe;
import net.emaze.dysfunctional.order.ComparableComparator;
import net.emaze.dysfunctional.order.SequencingPolicy;
import net.emaze.dysfunctional.tuples.Pair;


public interface IpPolicy extends SequencingPolicy<MyIp> {
    public static final BigInteger IPV4_TO_V6_PREFIX = new BigInteger("000000000000000000000FFFF00000000", 16);
    public static final BigInteger IPV4_TO_V6_MASK = new BigInteger("FFFFFFFFFFFFFFFFFFFFFFFF00000000", 16);
    public static final int IPV4_TO_V6_POPULATION = 96;

    MyIp getFirstIp();

    MyIp getLastIp();

    MyMask getNarrowestMask();

    MyMask getWidestMask();
    
    Ranges<MyIp> getRanges();

    BigInteger maxValue();

    BigInteger minValue();

    int maxPopulation();
    
    int compare(MyIp lhs, MyIp rhs);
    
    int compare(MyMask lhs, MyMask rhs);
    
    IpPolicy selectForComparison(IpPolicy other);

    public static class V6 implements IpPolicy {

        private static final BigInteger MIN_ADDRESS_IN_BITS = BigInteger.ZERO;
        private static final BigInteger MAX_ADDRESS_IN_BITS = new BigInteger("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF", 16);
        private static final int MAX_MASK_POPULATION = 128;
        private static final MyIp FIRST_IP = new MyIp(MIN_ADDRESS_IN_BITS, new V6());
        private static final MyIp LAST_IP = new MyIp(MAX_ADDRESS_IN_BITS, new V6());
        private static final MyMask NARROWEST = new MyMask(MAX_ADDRESS_IN_BITS, new V6());
        private static final MyMask WIDEST = new MyMask(MIN_ADDRESS_IN_BITS, new V6());
        private static final Ranges<MyIp> RANGES = new Ranges<>(new ComparableComparator<MyIp>(), new V6(), FIRST_IP);

        @Override
        public MyIp getFirstIp() {
            return FIRST_IP;
        }

        @Override
        public MyIp getLastIp() {
            return LAST_IP;
        }

        @Override
        public MyMask getNarrowestMask() {
            return NARROWEST;
        }

        @Override
        public MyMask getWidestMask() {
            return WIDEST;
        }

        @Override
        public Ranges<MyIp> getRanges() {
           return RANGES;
        }

        @Override
        public BigInteger maxValue() {
            return MAX_ADDRESS_IN_BITS;
        }

        @Override
        public BigInteger minValue() {
            return MIN_ADDRESS_IN_BITS;
        }
        
        @Override
        public int compare(MyIp lhs, MyIp rhs) {
            final BigInteger left = lhs.version() instanceof V6 ? lhs.bits() : IPV4_TO_V6_PREFIX.or(lhs.bits());
            final BigInteger right = rhs.version() instanceof V6 ? rhs.bits() : IPV4_TO_V6_PREFIX.or(rhs.bits());
            return left.compareTo(right);
        }
        
        @Override
        public int compare(MyMask lhs, MyMask rhs) {
            final BigInteger left = lhs.version() instanceof V6 ? lhs.bits() : IPV4_TO_V6_PREFIX.or(lhs.bits());
            final BigInteger right = rhs.version() instanceof V6 ? rhs.bits() : IPV4_TO_V6_PREFIX.or(rhs.bits());
            return left.compareTo(right);
        }
        
        @Override
        public IpPolicy selectForComparison(IpPolicy other) {
            return this;
        }

        public static MyIp toV6(MyIp source) {
            if (source.version() instanceof V6) {
                return source;
            }
            final BigInteger address = IPV4_TO_V6_PREFIX.or(source.bits());
            return new MyIp(address, new V6());
        }

        public static MyMask toV6(MyMask source) {
            if (source.policy instanceof V6) {
                return source;
            }
            final BigInteger mask = IPV4_TO_V6_MASK.or(source.bits());
            return new MyMask(mask, new IpPolicy.V6());
        }

        public static MyNetwork toV6(MyNetwork source) {
            if (source.version() instanceof V6) {
                return source;
            }
            final Pair<MyIp, MyMask> cidr = source.toCidr();
            return new MyNetwork(toV6(cidr.first()), toV6(cidr.second()), new V6());
        }

        @Override
        public int maxPopulation() {
            return MAX_MASK_POPULATION;
        }

        @Override
        public Maybe<MyIp> next(MyIp ip) {
            if (LAST_IP.equals(ip)) {
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

        private static final BigInteger MIN_ADDRESS_IN_BITS = BigInteger.ZERO;
        private static final BigInteger MAX_ADDRESS_IN_BITS = new BigInteger("FFFFFFFF", 16);
        private static final int MAX_MASK_POPULATION = 32;
        private static final MyIp FIRST_IP = new MyIp(MIN_ADDRESS_IN_BITS, new V4());
        private static final MyIp LAST_IP = new MyIp(MAX_ADDRESS_IN_BITS, new V4());
        private static final MyMask NARROWEST = new MyMask(MAX_ADDRESS_IN_BITS, new V4());
        private static final MyMask WIDEST = new MyMask(MIN_ADDRESS_IN_BITS, new V4());
        private static final Ranges<MyIp> RANGES = new Ranges<>(new ComparableComparator<MyIp>(), new V4(), FIRST_IP);

        @Override
        public MyIp getFirstIp() {
            return FIRST_IP;
        }

        @Override
        public MyIp getLastIp() {
            return LAST_IP;
        }

        @Override
        public MyMask getNarrowestMask() {
            return NARROWEST;
        }

        @Override
        public MyMask getWidestMask() {
            return WIDEST;
        }
        
        @Override
        public Ranges<MyIp> getRanges() {
           return RANGES;
        }

        @Override
        public BigInteger maxValue() {
            return MAX_ADDRESS_IN_BITS;
        }

        @Override
        public BigInteger minValue() {
            return MIN_ADDRESS_IN_BITS;
        }
        
        @Override
        public int compare(MyIp lhs, MyIp rhs) {
            dbc.precondition((lhs.version() instanceof V4) && (rhs.version() instanceof V4), "Use V6 to compare V4 with V6");
            return lhs.bits().compareTo(rhs.bits());
        }
        
        @Override
        public int compare(MyMask lhs, MyMask rhs) {
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

        public static MyIp toV4(MyIp source) {
            if (source.version() instanceof V4) {
                return source;
            }
            dbc.precondition(source.bits().and(IPV4_TO_V6_MASK).equals(IPV4_TO_V6_PREFIX), "Address cannot be converted to IPv4");
            final BigInteger address = source.bits().andNot(IPV4_TO_V6_MASK);
            return new MyIp(address, new V4());
        }

        public static MyMask toV4(MyMask source) {
            if (source.policy instanceof V4) {
                return source;
            }
            dbc.precondition(source.bits().and(IPV4_TO_V6_MASK).equals(IPV4_TO_V6_MASK), "Mask cannot be converted to IPv4");
            final BigInteger mask = source.bits().andNot(IPV4_TO_V6_MASK);
            return new MyMask(mask, new IpPolicy.V4());
        }

        public static MyNetwork toV4(MyNetwork source) {
            if (source.version() instanceof V4) {
                return source;
            }
            final Pair<MyIp, MyMask> cidr = source.toCidr();
            return new MyNetwork(toV4(cidr.first()), toV4(cidr.second()), new V4());
        }

        @Override
        public int maxPopulation() {
            return MAX_MASK_POPULATION;
        }

        @Override
        public Maybe<MyIp> next(MyIp ip) {
            if (LAST_IP.equals(ip)) {
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
