package net.emaze.networks.jackson;

import com.fasterxml.jackson.databind.ObjectMapper;
import java.io.IOException;
import net.emaze.networks.ipv4.Ipv4;
import org.junit.Assert;
import org.junit.Test;

public class Ipv4ToStringTest {

    @Test
    public void testSerialization() throws IOException {
        final String expected = "{'ip':'127.0.0.1'}".replace("'", "\"");
        final ObjectMapper mapper = new ObjectMapper();
        mapper.registerModule(new NetworksModule());
        final BeanWithIpv4 bwi = new BeanWithIpv4();
        bwi.setIp(Ipv4.parse("127.0.0.1"));
        final String got = mapper.writeValueAsString(bwi);
        Assert.assertEquals(expected, got);
    }

}
